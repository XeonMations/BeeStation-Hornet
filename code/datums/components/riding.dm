/datum/component/riding
	var/last_vehicle_move = 0 //used for move delays
	var/last_move_diagonal = FALSE
	var/vehicle_move_delay = 2 //tick delay between movements, lower = faster, higher = slower
	var/keytype
	var/vehicle_move_multiplier = 1

	var/slowed = FALSE
	var/slowvalue = 1

	var/list/riding_offsets = list()	//position_of_user = list(dir = list(px, py)), or RIDING_OFFSET_ALL for a generic one.
	var/list/directional_vehicle_layers = list()	//["[DIRECTION]"] = layer. Don't set it for a direction for default, set a direction to null for no change.
	var/list/directional_vehicle_offsets = list()	//same as above but instead of layer you have a list(px, py)
	var/list/allowed_turf_typecache
	var/list/forbid_turf_typecache					//allow typecache for only certain turfs, forbid to allow all but those. allow only certain turfs will take precedence.
	var/allow_one_away_from_valid_turf = TRUE		//allow moving one tile away from a valid turf but not more.
	var/override_allow_spacemove = FALSE
	var/drive_verb = "drive"
	var/ride_check_rider_incapacitated = FALSE
	var/ride_check_rider_restrained = FALSE
	var/ride_check_ridden_incapacitated = FALSE
	var/ride_check_ridden_restrained = FALSE

	var/del_on_unbuckle_all = FALSE
	var/emped = FALSE
	var/empable = FALSE

/datum/component/riding/Initialize()
	if(!ismovable(parent))
		return COMPONENT_INCOMPATIBLE
	RegisterSignal(parent, COMSIG_ATOM_DIR_CHANGE, PROC_REF(vehicle_turned))
	RegisterSignal(parent, COMSIG_MOVABLE_BUCKLE, PROC_REF(vehicle_mob_buckle))
	RegisterSignal(parent, COMSIG_MOVABLE_UNBUCKLE, PROC_REF(vehicle_mob_unbuckle))
	RegisterSignal(parent, COMSIG_MOVABLE_MOVED, PROC_REF(vehicle_moved))
	RegisterSignal(parent, COMSIG_ATOM_EMP_ACT, PROC_REF(on_emp_act))
	//Calculate the move multiplier speed, to be proportional to mob speed
	vehicle_move_multiplier = CONFIG_GET(number/movedelay/run_delay) / 1.5

/datum/component/riding/proc/vehicle_mob_unbuckle(datum/source, mob/living/M, force = FALSE)
	SIGNAL_HANDLER

	unequip_buckle_inhands(parent)
	var/atom/movable/AM = parent
	restore_position(M)
	unequip_buckle_inhands(M)
	M.updating_glide_size = TRUE
	if(del_on_unbuckle_all && !AM.has_buckled_mobs())
		qdel(src)

/datum/component/riding/proc/vehicle_mob_buckle(datum/source, mob/living/M, force = FALSE)
	SIGNAL_HANDLER

	var/atom/movable/movable_parent = parent
	M.set_glide_size(movable_parent.glide_size)
	M.updating_glide_size = FALSE
	handle_vehicle_offsets(movable_parent.dir)

/datum/component/riding/proc/handle_vehicle_layer(dir)
	var/atom/movable/AM = parent
	var/static/list/defaults = list(TEXT_NORTH = OBJ_LAYER, TEXT_SOUTH = ABOVE_MOB_LAYER, TEXT_EAST = ABOVE_MOB_LAYER, TEXT_WEST = ABOVE_MOB_LAYER)
	. = defaults["[dir]"]
	if(directional_vehicle_layers["[dir]"])
		. = directional_vehicle_layers["[dir]"]
	if(isnull(.))	//you can set it to null to not change it.
		. = AM.layer
	AM.layer = .

/datum/component/riding/proc/set_vehicle_dir_layer(dir, layer)
	directional_vehicle_layers["[dir]"] = layer

/datum/component/riding/proc/vehicle_moved(datum/source, dir)
	SIGNAL_HANDLER

	var/atom/movable/movable_parent = parent
	if (isnull(dir))
		dir = movable_parent.dir
	movable_parent.set_glide_size(DELAY_TO_GLIDE_SIZE(vehicle_move_delay))
	for (var/m in movable_parent.buckled_mobs)
		ride_check(m)
		var/mob/buckled_mob = m
		buckled_mob.set_glide_size(movable_parent.glide_size)
	handle_vehicle_offsets(dir)
	handle_vehicle_layer(dir)

/datum/component/riding/proc/vehicle_turned(datum/source, _old_dir, new_dir)
	vehicle_moved(source, new_dir)

/datum/component/riding/proc/ride_check(mob/living/M)
	var/atom/movable/AM = parent
	var/mob/AMM = AM
	var/kick_us_off
	if((ride_check_rider_restrained && HAS_TRAIT(M, TRAIT_RESTRAINED)) || (ride_check_rider_incapacitated && M.incapacitated(IGNORE_RESTRAINTS|IGNORE_GRAB)))
		kick_us_off = TRUE
	if(kick_us_off || (istype(AMM) && ((ride_check_ridden_restrained && HAS_TRAIT(AMM, TRAIT_RESTRAINED)) || (ride_check_ridden_incapacitated && AMM.incapacitated(IGNORE_RESTRAINTS|IGNORE_GRAB)))))
		M.visible_message(span_warning("[M] falls off of [AM]!"), \
						span_warning("You fall off of [AM]!"))
		AM.unbuckle_mob(M)
	return TRUE

/datum/component/riding/proc/force_dismount(mob/living/M)
	var/atom/movable/AM = parent
	AM.unbuckle_mob(M)

/datum/component/riding/proc/handle_vehicle_offsets(dir)
	var/atom/movable/AM = parent
	var/AM_dir = "[dir]"
	var/passindex = 0
	if(AM.has_buckled_mobs())
		for(var/m in AM.buckled_mobs)
			passindex++
			var/mob/living/buckled_mob = m
			var/list/offsets = get_offsets(passindex)
			buckled_mob.setDir(dir)
			dir_loop:
				for(var/offsetdir in offsets)
					if(offsetdir == AM_dir)
						var/list/diroffsets = offsets[offsetdir]
						buckled_mob.pixel_x = diroffsets[1]
						if(diroffsets.len >= 2)
							buckled_mob.pixel_y = diroffsets[2]
						if(diroffsets.len == 3)
							buckled_mob.layer = diroffsets[3]
						break dir_loop
	var/list/static/default_vehicle_pixel_offsets = list(TEXT_NORTH = list(0, 0), TEXT_SOUTH = list(0, 0), TEXT_EAST = list(0, 0), TEXT_WEST = list(0, 0))
	var/px = default_vehicle_pixel_offsets[AM_dir]
	var/py = default_vehicle_pixel_offsets[AM_dir]
	if(directional_vehicle_offsets[AM_dir])
		if(isnull(directional_vehicle_offsets[AM_dir]))
			px = AM.pixel_x
			py = AM.pixel_y
		else
			px = directional_vehicle_offsets[AM_dir][1]
			py = directional_vehicle_offsets[AM_dir][2]
	AM.pixel_x = px
	AM.pixel_y = py

/datum/component/riding/proc/set_vehicle_dir_offsets(dir, x, y)
	directional_vehicle_offsets["[dir]"] = list(x, y)

//Override this to set your vehicle's various pixel offsets
/datum/component/riding/proc/get_offsets(pass_index) // list(dir = x, y, layer)
	. = list(TEXT_NORTH = list(0, 0), TEXT_SOUTH = list(0, 0), TEXT_EAST = list(0, 0), TEXT_WEST = list(0, 0))
	if(riding_offsets["[pass_index]"])
		. = riding_offsets["[pass_index]"]
	else if(riding_offsets["[RIDING_OFFSET_ALL]"])
		. = riding_offsets["[RIDING_OFFSET_ALL]"]

/datum/component/riding/proc/set_riding_offsets(index, list/offsets)
	if(!islist(offsets))
		return FALSE
	riding_offsets["[index]"] = offsets

//KEYS
/datum/component/riding/proc/keycheck(mob/user)
	return !keytype || user.is_holding_item_of_type(keytype)

//BUCKLE HOOKS
/datum/component/riding/proc/restore_position(mob/living/buckled_mob)
	if(buckled_mob)
		buckled_mob.pixel_x = buckled_mob.base_pixel_x
		buckled_mob.pixel_y = buckled_mob.base_pixel_y
		if(buckled_mob.client)
			buckled_mob.client.view_size.resetToDefault()

//MOVEMENT
/datum/component/riding/proc/turf_check(turf/next, turf/current)
	if(allowed_turf_typecache && !allowed_turf_typecache[next.type])
		return (allow_one_away_from_valid_turf && allowed_turf_typecache[current.type])
	else if(forbid_turf_typecache && forbid_turf_typecache[next.type])
		return (allow_one_away_from_valid_turf && !forbid_turf_typecache[current.type])
	return TRUE

/datum/component/riding/proc/handle_ride(mob/user, direction)
	var/atom/movable/AM = parent
	if(user.incapacitated())
		Unbuckle(user)
		return

	if(world.time < last_vehicle_move + ((last_move_diagonal? sqrt(2) : 1) * vehicle_move_delay * vehicle_move_multiplier))
		return
	last_vehicle_move = world.time

	if(emped && empable)
		to_chat(user, span_notice("\The [AM]'s controls aren't responding!"))
		return
	if(keycheck(user))
		var/turf/next = get_step(AM, direction)
		var/turf/current = get_turf(AM)
		if(!istype(next) || !istype(current))
			return	//not happening.
		if(!turf_check(next, current))
			to_chat(user, "\The [AM] cannot go onto [next]!")
			return
		if(!Process_Spacemove(direction) || !isturf(AM.loc))
			return
		if(!(direction & UP) && !(direction & DOWN))
			step(AM, direction)
		else if(ismob(AM))
			var/mob/M = AM
			var/old_dir = M.dir
			M.zMove((direction & UP) ? UP : DOWN, feedback = TRUE, feedback_to = user)
			M.setDir(old_dir)

		if((direction & (direction - 1)) && (AM.loc == next))		//moved diagonally
			last_move_diagonal = TRUE
		else
			last_move_diagonal = FALSE

		handle_vehicle_layer(AM.dir)
		handle_vehicle_offsets(AM.dir)
	else
		to_chat(user, span_notice("You'll need the keys in one of your hands to [drive_verb] [AM]."))

/datum/component/riding/proc/Unbuckle(atom/movable/M)
	addtimer(CALLBACK(parent, TYPE_PROC_REF(/atom/movable, unbuckle_mob), M), 0, TIMER_UNIQUE)

/datum/component/riding/proc/Process_Spacemove(direction)
	var/atom/movable/AM = parent
	return override_allow_spacemove || AM.has_gravity()

/datum/component/riding/proc/account_limbs(mob/living/M)
	if(M.usable_legs < 2 && !slowed)
		vehicle_move_delay = vehicle_move_delay + slowvalue
		slowed = TRUE
	else if(slowed)
		vehicle_move_delay = vehicle_move_delay - slowvalue
		slowed = FALSE

///////Yes, I said humans. No, this won't end well...//////////
/datum/component/riding/human
	del_on_unbuckle_all = TRUE

/datum/component/riding/human/Initialize()
	. = ..()
	RegisterSignal(parent, COMSIG_HUMAN_MELEE_UNARMED_ATTACK, PROC_REF(on_host_unarmed_melee))

/datum/component/riding/human/vehicle_mob_unbuckle(datum/source, mob/living/M, force = FALSE)
	var/mob/living/carbon/human/H = parent
	H.remove_movespeed_modifier(/datum/movespeed_modifier/human_carry)
	. = ..()

/datum/component/riding/human/vehicle_mob_buckle(datum/source, mob/living/M, force = FALSE)
	. = ..()
	var/mob/living/carbon/human/H = parent
	H.add_movespeed_modifier(/datum/movespeed_modifier/human_carry)

/datum/component/riding/human/proc/on_host_unarmed_melee(mob/living/carbon/human/human_parent, atom/target, proximity, modifiers)
	SIGNAL_HANDLER

	if(LAZYACCESS(modifiers, RIGHT_CLICK) && (target in human_parent.buckled_mobs))
		force_dismount(target)
		return COMPONENT_CANCEL_ATTACK_CHAIN

/datum/component/riding/human/handle_vehicle_layer(dir)
	var/atom/movable/AM = parent
	if(AM.buckled_mobs?.len)
		for(var/mob/M in AM.buckled_mobs) //ensure proper layering of piggyback and carry, sometimes weird offsets get applied
			M.layer = MOB_LAYER
		if(!AM.buckle_lying)
			if(dir == SOUTH)
				AM.layer = ABOVE_MOB_LAYER
			else
				AM.layer = OBJ_LAYER
		else
			if(dir == NORTH)
				AM.layer = OBJ_LAYER
			else
				AM.layer = ABOVE_MOB_LAYER
	else
		AM.layer = MOB_LAYER

/datum/component/riding/human/get_offsets(pass_index)
	var/mob/living/carbon/human/H = parent
	if(H.buckle_lying)
		return list(TEXT_NORTH = list(0, 6), TEXT_SOUTH = list(0, 6), TEXT_EAST = list(0, 6), TEXT_WEST = list(0, 6))
	else
		return list(TEXT_NORTH = list(0, 6), TEXT_SOUTH = list(0, 6), TEXT_EAST = list(-6, 4), TEXT_WEST = list( 6, 4))


/datum/component/riding/human/force_dismount(mob/living/user)
	var/atom/movable/AM = parent
	AM.unbuckle_mob(user)
	user.Paralyze(60)
	user.visible_message(span_warning("[AM] pushes [user] off of [AM.p_them()]!"), \
						span_warning("[AM] pushes you off of [AM.p_them()]!"))

/datum/component/riding/cyborg
	del_on_unbuckle_all = TRUE

/datum/component/riding/cyborg/ride_check(mob/user)
	var/atom/movable/AM = parent
	if(user.incapacitated())
		var/kick = TRUE
		if(iscyborg(AM))
			var/mob/living/silicon/robot/R = AM
			if(R.module && R.module.ride_allow_incapacitated)
				kick = FALSE
		if(kick)
			to_chat(user, span_userdanger("You fall off of [AM]!"))
			Unbuckle(user)
			return
	if(iscarbon(user))
		var/mob/living/carbon/carbonuser = user
		if(!carbonuser.usable_hands)
			Unbuckle(user)
			to_chat(user, span_userdanger("You can't grab onto [AM] with no hands!"))
			return

/datum/component/riding/cyborg/handle_vehicle_layer(dir)
	var/atom/movable/AM = parent
	if(AM.buckled_mobs && AM.buckled_mobs.len)
		if(dir == SOUTH)
			AM.layer = ABOVE_MOB_LAYER
		else
			AM.layer = OBJ_LAYER
	else
		AM.layer = MOB_LAYER

/datum/component/riding/cyborg/get_offsets(pass_index) // list(dir = x, y, layer)
	return list(TEXT_NORTH = list(0, 4), TEXT_SOUTH = list(0, 4), TEXT_EAST = list(-6, 3), TEXT_WEST = list( 6, 3))

/datum/component/riding/cyborg/handle_vehicle_offsets(dir)
	var/atom/movable/AM = parent
	if(AM.has_buckled_mobs())
		for(var/mob/living/M in AM.buckled_mobs)
			M.setDir(dir)
			if(iscyborg(AM))
				var/mob/living/silicon/robot/R = AM
				if(istype(R.module))
					M.pixel_x = R.module.ride_offset_x[dir2text(dir)]
					M.pixel_y = R.module.ride_offset_y[dir2text(dir)]
			else
				..()

/datum/component/riding/cyborg/force_dismount(mob/living/M)
	var/atom/movable/AM = parent
	AM.unbuckle_mob(M)
	var/turf/target = get_edge_target_turf(AM, AM.dir)
	var/turf/targetm = get_step(get_turf(AM), AM.dir)
	M.Move(targetm)
	M.visible_message(span_warning("[M] is thrown clear of [AM]!"), \
					span_warning("You're thrown clear of [AM]!"))
	M.throw_at(target, 14, 5, AM)
	M.Knockdown(60)

/datum/component/riding/proc/equip_buckle_inhands(mob/living/carbon/human/user, amount_required = 1, riding_target_override = null)
	var/atom/movable/AM = parent
	var/amount_equipped = 0
	for(var/amount_needed = amount_required, amount_needed > 0, amount_needed--)
		var/obj/item/riding_offhand/inhand = new /obj/item/riding_offhand(user)
		if(!riding_target_override)
			inhand.rider = user
		else
			inhand.rider = riding_target_override
		inhand.parent = AM
		if(user.put_in_hands(inhand, TRUE))
			amount_equipped++
		else
			break
	if(amount_equipped >= amount_required)
		return TRUE
	else
		unequip_buckle_inhands(user)
		return FALSE

/datum/component/riding/proc/unequip_buckle_inhands(mob/living/carbon/user)
	var/atom/movable/AM = parent
	for(var/obj/item/riding_offhand/O in user.contents)
		if(O.parent != AM)
			CRASH("RIDING OFFHAND ON WRONG MOB")
		if(O.selfdeleting)
			continue
		else
			qdel(O)
	return TRUE

/obj/item/riding_offhand
	name = "offhand"
	icon = 'icons/obj/items_and_weapons.dmi'
	icon_state = "offhand"
	w_class = WEIGHT_CLASS_HUGE
	item_flags = ABSTRACT | DROPDEL | NOBLUDGEON
	resistance_flags = INDESTRUCTIBLE | LAVA_PROOF | FIRE_PROOF | UNACIDABLE | ACID_PROOF
	var/mob/living/carbon/rider
	var/mob/living/parent
	var/selfdeleting = FALSE

/obj/item/riding_offhand/dropped()
	selfdeleting = TRUE
	..()

/obj/item/riding_offhand/equipped()
	if(loc != rider && loc != parent)
		selfdeleting = TRUE
		qdel(src)
	. = ..()

/obj/item/riding_offhand/Destroy()
	var/atom/movable/AM = parent
	if(selfdeleting)
		if(rider in AM.buckled_mobs)
			AM.unbuckle_mob(rider)
	. = ..()

//tamed riding
/datum/component/riding/tamed/Initialize()
	. = ..()
	if(istype(parent, /mob/living/simple_animal))
		var/mob/living/simple_animal/S = parent
		override_allow_spacemove = S.spacewalk
		RegisterSignal(parent, COMSIG_MOB_DEATH, PROC_REF(handle_mortality))

/datum/component/riding/tamed/proc/handle_mortality()
	qdel(src)

/datum/component/riding/tamed/vehicle_mob_buckle(datum/source, mob/living/M, force = FALSE)
	if(istype(parent, /mob/living/simple_animal))
		var/mob/living/simple_animal/S = parent
		M.spacewalk = S.spacewalk
		S.toggle_ai(AI_OFF)
	..()

/datum/component/riding/tamed/vehicle_mob_unbuckle(datum/source, mob/living/M, force = FALSE)
	M.spacewalk = FALSE
	if(istype(parent, /mob/living/simple_animal))
		var/mob/living/simple_animal/S = parent
		S.toggle_ai(AI_ON)
	..()

/datum/component/riding/proc/on_emp_act(datum/source, severity)
	SIGNAL_HANDLER

	if(!empable)
		return
	emped = TRUE
	var/atom/movable/AM = parent
	AM.add_emitter(/obj/emitter/fire_smoke, "smoke")
	addtimer(CALLBACK(src, PROC_REF(reboot)), 300 / severity, TIMER_UNIQUE|TIMER_OVERRIDE) //if a new EMP happens, remove the old timer so it doesn't reactivate early

/datum/component/riding/proc/reboot()
	emped = FALSE
	var/atom/movable/AM = parent
	AM.remove_emitter("smoke")
