/atom/movable
	layer = OBJ_LAYER
	appearance_flags = TILE_BOUND | PIXEL_SCALE
	// Movement related vars
	step_size = 8
	glide_size = 8
	//PIXEL MOVEMENT VARS
	/// stores fractional pixel movement in the x
	var/fx
	/// stores fractional pixel movement in the y
	var/fy
	/// velocity in x
	var/vx = 0
	/// velocity in y
	var/vy = 0
	/// acceleration
	var/accel = 1
	/// deceleration
	var/decel = 2
	/// maxspeed
	var/maxspeed = 6
	/// velocity dir
	var/vdir = NONE
	/// sidestepper?
	var/can_sidestep = FALSE
	/// collider for sidestepping
	var/atom/movable/collider/slider/slider

	var/walking = NONE
	var/move_resist = MOVE_RESIST_DEFAULT
	var/move_force = MOVE_FORCE_DEFAULT
	var/pull_force = PULL_FORCE_DEFAULT
	///whether we are already sidestepping or not
	var/sidestep = FALSE

	//Misc
	var/last_move = null
	var/last_move_time = 0
	var/anchored = FALSE
	var/datum/thrownthing/throwing = null
	var/throw_speed = 2 //How many tiles to move per ds when being thrown. Float values are fully supported
	var/throw_range = 7
	var/mob/pulledby = null
	/// What language holder type to init as
	var/initial_language_holder = /datum/language_holder
	/// Holds all languages this mob can speak and understand
	VAR_PRIVATE/datum/language_holder/language_holder

	var/verb_say = "says"
	var/verb_ask = "asks"
	var/verb_exclaim = "exclaims"
	var/verb_whisper = "whispers"
	var/verb_sing = "sings"
	var/verb_yell = "yells"
	var/speech_span
	var/inertia_moving = FALSE
	/// Things we can pass through while moving. If any of this matches the thing we're trying to pass's [pass_flags_self], then we can pass through.
	var/pass_flags = NONE
	/// If false makes CanPass call CanPassThrough on this type instead of using default behaviour
	var/generic_canpass = TRUE
	var/atom/movable/moving_from_pull		//attempt to resume grab after moving instead of before.
	///Holds information about any movement loops currently running/waiting to run on the movable. Lazy, will be null if nothing's going on
	var/datum/movement_packet/move_packet
	var/list/acted_explosions	//for explosion dodging
	var/datum/forced_movement/force_moving = null	//handled soley by forced_movement.dm
	/**
	  * In case you have multiple types, you automatically use the most useful one.
	  * IE: Skating on ice, flippers on water, flying over chasm/space, etc.
	  * I reccomend you use the movetype_handler system and not modify this directly, especially for living mobs.
	  */
	var/movement_type = GROUND

	var/atom/movable/pulling
	var/grab_state = 0
	var/throwforce = 0
	var/datum/component/orbiter/orbiting
	var/can_be_z_moved = TRUE

	/// Either FALSE, [EMISSIVE_BLOCK_GENERIC], or [EMISSIVE_BLOCK_UNIQUE]
	var/blocks_emissive = FALSE
	///Internal holder for emissive blocker object, do not use directly use blocks_emissive
	var/atom/movable/emissive_blocker/em_block

	///how bounds handle rotation
	var/brotation = BOUNDS_SIMPLE_ROTATE
	/**
	 * an associative lazylist of relevant nested contents by "channel", the list is of the form: list(channel = list(important nested contents of that type))
	 * each channel has a specific purpose and is meant to replace potentially expensive nested contents iteration
	 * do NOT add channels to this for little reason as it can add considerable memory usage.
	 */
	var/list/important_recursive_contents
	///contains every client mob corresponding to every client eye in this container. lazily updated by SSparallax and is sparse:
	///only the last container of a client eye has this list assuming no movement since SSparallax's last fire
	var/list/client_mobs_in_contents
	///Lazylist to keep track on the sources of illumination.
	var/list/affected_dynamic_lights
	///Highest-intensity light affecting us, which determines our visibility.
	var/affecting_dynamic_lumi = 0

	/// Whether this atom should have its dir automatically changed when it moves. Setting this to FALSE allows for things such as directional windows to retain dir on moving without snowflake code all of the place.
	var/set_dir_on_move = TRUE


/atom/movable/Initialize(mapload)
	. = ..()
	set_sidestep(can_sidestep) // creates slider if there isn't one already
	update_bounds(olddir=NORTH, newdir=dir) // bounds assume north but some things arent north by default for some god knows reason
	if(opacity) // makes opaque objects not block entire tiles
		appearance_flags &= ~TILE_BOUND
	switch(blocks_emissive)
		if(EMISSIVE_BLOCK_GENERIC)
			var/mutable_appearance/gen_emissive_blocker = mutable_appearance(icon, icon_state, layer, EMISSIVE_PLANE)
			gen_emissive_blocker.dir = dir
			gen_emissive_blocker.appearance_flags = EMISSIVE_APPEARANCE_FLAGS
			gen_emissive_blocker.color = GLOB.em_blocker_matrix
			add_overlay(list(gen_emissive_blocker))
		if(EMISSIVE_BLOCK_UNIQUE)
			render_target = ref(src)
			em_block = new(src, render_target)
			update_appearance(UPDATE_OVERLAYS)
	if(opacity)
		AddElement(/datum/element/light_blocking)
	switch(light_system)
		if(MOVABLE_LIGHT)
			AddComponent(/datum/component/overlay_lighting)
		if(MOVABLE_LIGHT_DIRECTIONAL)
			AddComponent(/datum/component/overlay_lighting, is_directional = TRUE)

	if(isturf(loc))
		var/turf/T = loc
		T.update_above() // Z-Mimic


/atom/movable/Destroy(force)
	QDEL_NULL(language_holder)
	QDEL_NULL(em_block)
	if(bound_overlay)
		QDEL_NULL(bound_overlay)

	unbuckle_all_mobs(force = TRUE)

	if(loc)
		//Restore air flow if we were blocking it (movables with ATMOS_PASS_PROC will need to do this manually if necessary)
		if(((can_atmos_pass == ATMOS_PASS_DENSITY && density) || can_atmos_pass == ATMOS_PASS_NO) && isturf(loc))
			can_atmos_pass = ATMOS_PASS_YES
			air_update_turf(TRUE, FALSE)
		loc.handle_atom_del(src)

	if(opacity)
		RemoveElement(/datum/element/light_blocking)

	invisibility = INVISIBILITY_ABSTRACT

	if(pulledby)
		pulledby.stop_pulling()
	if(pulling)
		stop_pulling()

	if(orbiting)
		orbiting.end_orbit(src)
		orbiting = null

	if(move_packet)
		if(!QDELETED(move_packet))
			qdel(move_packet)
		move_packet = null

	if(important_recursive_contents && (important_recursive_contents[RECURSIVE_CONTENTS_CLIENT_MOBS] || important_recursive_contents[RECURSIVE_CONTENTS_HEARING_SENSITIVE]))
		SSspatial_grid.force_remove_from_cell(src)

	LAZYCLEARLIST(client_mobs_in_contents)

	. = ..()

	for(var/movable_content in contents)
		qdel(movable_content)

	moveToNullspace()

	//This absolutely must be after moveToNullspace()
	//We rely on Entered and Exited to manage this list, and the copy of this list that is on any /atom/movable "Containers"
	//If we clear this before the nullspace move, a ref to this object will be hung in any of its movable containers
	LAZYCLEARLIST(important_recursive_contents)


	vis_locs = null //clears this atom out of all viscontents

	// Checking length(vis_contents) before cutting has significant speed benefits
	if (length(vis_contents))
		vis_contents.Cut()

/atom/movable/proc/update_emissive_block()
	if(!blocks_emissive)
		return
	else if (blocks_emissive == EMISSIVE_BLOCK_GENERIC)
		var/mutable_appearance/gen_emissive_blocker = mutable_appearance(icon, icon_state, layer, EMISSIVE_PLANE)
		gen_emissive_blocker.dir = dir
		gen_emissive_blocker.appearance_flags = EMISSIVE_APPEARANCE_FLAGS
		gen_emissive_blocker.color = GLOB.em_blocker_matrix
		return gen_emissive_blocker
	else if(blocks_emissive == EMISSIVE_BLOCK_UNIQUE)
		if(!em_block && !QDELETED(src))
			render_target = ref(src)
			em_block = new(src, render_target)
		return em_block

/atom/movable/proc/set_sidestep(val = 0)
	can_sidestep = val
	if(can_sidestep && !slider)
		slider = new()
	else if(!can_sidestep && slider)
		qdel(slider)
	return can_sidestep

/atom/movable/update_overlays()
	. = ..()
	. += update_emissive_block()

/atom/movable/vv_edit_var(var_name, var_value)
	switch(var_name)
		if(NAMEOF(src, anchored))
			set_anchored(var_value)
			return TRUE
		if(NAMEOF(src, x))
			var/turf/T = locate(var_value, y, z)
			if(T)
				forceMove(T)
				return TRUE
			return FALSE
		if(NAMEOF(src, y))
			var/turf/T = locate(x, var_value, z)
			if(T)
				forceMove(T)
				return TRUE
			return FALSE
		if(NAMEOF(src, z))
			var/turf/T = locate(x, y, var_value)
			if(T)
				forceMove(T)
				return TRUE
			return FALSE
		if(NAMEOF(src, loc))
			if(istype(var_value, /atom))
				forceMove(var_value)
				return TRUE
			else if(isnull(var_value))
				moveToNullspace()
				return TRUE
			return FALSE
	return ..()

/atom/movable/proc/start_pulling(atom/movable/AM, state, force = move_force, supress_message = FALSE)
	if(QDELETED(AM))
		return FALSE
	if(!(AM.can_be_pulled(src, state, force)))
		return FALSE

	// If we're pulling something then drop what we're currently pulling and pull this instead.
	if(pulling)
		if(state == 0)
			stop_pulling()
			return FALSE
		// Are we trying to pull something we are already pulling? Then enter grab cycle and end.
		if(AM == pulling)
			setGrabState(state)
			if(istype(AM,/mob/living))
				var/mob/living/AMob = AM
				AMob.grabbedby(src)
			return TRUE
		stop_pulling()
	if(AM.pulledby)
		log_combat(AM, AM.pulledby, "pulled from", src, important = FALSE)
		AM.pulledby.stop_pulling() //an object can't be pulled by two mobs at once.
	pulling = AM
	AM.set_pulledby(src)
	setGrabState(state)
	if(!isliving(pulling))
		pulling.set_sidestep(TRUE)
	if(ismob(AM))
		var/mob/M = AM
		M.update_movespeed() // set the proper step_size
		log_combat(src, M, "grabbed", addition="passive grab", important = FALSE)
		if(!supress_message)
			M.visible_message(span_warning("[src] grabs [M] passively."), \
				span_danger("[src] grabs you passively."))
	SEND_SIGNAL(pulling, COMSIG_MOVABLE_PULLED)
	return TRUE

/atom/movable/proc/stop_pulling()
	if(pulling)
		if(ismob(pulling?.pulledby))
			pulling.pulledby.log_message("has stopped pulling [key_name(pulling)]", LOG_ATTACK)
			pulling.pulledby.update_movespeed() // set their movespeed to the usual
		if(ismob(pulling))
			pulling.log_message("has stopped being pulled by [key_name(pulling.pulledby)]", LOG_ATTACK)
		pulling.set_pulledby(null)
		if(!isliving(pulling))
			pulling.set_sidestep(initial(pulling.can_sidestep))
		var/atom/movable/ex_pulled = pulling
		setGrabState(GRAB_PASSIVE)
		pulling = null
		SEND_SIGNAL(ex_pulled, COMSIG_MOVABLE_NO_LONGER_PULLED)

///Reports the event of the change in value of the pulledby variable.
/atom/movable/proc/set_pulledby(new_pulledby)
	if(new_pulledby == pulledby)
		return FALSE //null signals there was a change, be sure to return FALSE if none happened here.
	. = pulledby
	pulledby = new_pulledby

/atom/movable/proc/Move_Pulled(atom/A, params)
	if(!check_pulling())
		return
	if(!Adjacent(A))
		to_chat(src, "<span class='warning'>You can't move [pulling] that far!</span>")
		return
	if(ismovable(pulling))
		pulling.step_size = 1 + pulling.Move(get_turf(A), get_dir(pulling.loc, A)) // 1 + pixels moved
		pulling.step_size = step_size
	return TRUE

/mob/living/Move_Pulled(atom/A, params)
	. = ..()
	if(!. || !isliving(A))
		return
	var/mob/living/L = A
	set_pull_offsets(L, grab_state)

/atom/movable/proc/check_pulling()
	. = FALSE
	if(!pulling)
		if(pulledby && bounds_dist(src, pulledby) > 32)
			pulledby.stop_pulling()
		return
	if(bounds_dist(src, pulling) > 32)
		stop_pulling()
		return
	if(!isturf(loc))
		stop_pulling()
		return
	if(pulling.anchored || pulling.move_resist > move_force)
		stop_pulling()
		return
	if(isliving(pulling))
		var/mob/living/liv = pulling
		if(liv.buckled?.buckle_prevents_pull) //if they're buckled to something that disallows pulling, prevent it
			stop_pulling()
			return
	return TRUE

#define ANGLE_ADJUST 10
/**
  * Handles the movement of the object src is pulling
  *
  * Tries to correct the pulled object if it's stuck
  * uses degstep to move the pulled object at an angle
  */
/atom/movable/proc/handle_pulled_movement()
	if(!pulling)
		return FALSE
	if(pulling.anchored)
		return FALSE
	if(pulling.move_resist > move_force)
		return FALSE
	var/distance = bounds_dist(src, pulling)
	if(distance < 6)
		return FALSE
	var/angle = GET_DEG(pulling, src)
	if((angle % 45) > 1) // We arent directly on a cardinal from the thing
		var/tempA = WRAP(angle, 0, 45)
		if(tempA >= 22.5)
			angle += min(ANGLE_ADJUST, 45-tempA)
		else
			angle -= min(ANGLE_ADJUST, tempA)
	angle = SIMPLIFY_DEGREES(angle)
	var/direct = angle2dir(angle)
	var/mspeed = vx
	if(direct & NORTH || direct & SOUTH)
		if(direct & EAST || direct & WEST)
			mspeed = CEILING(sqrt((abs(vx) ^ 2) * (abs(vy) ^ 2)), 1)
		else
			mspeed = vy
	pulling.add_velocity(direct, (abs(mspeed) + distance-6), TRUE)
/* 	if(!degstep(pulling, angle, distance-6))
		for(var/i in GLOB.cardinals)
			if(direct & i)
				if(step(pulling, i))
					return TRUE */
	return FALSE

#undef ANGLE_ADJUST

/**
  * Checks the distance between the object we're pulling before moving
  *
  * Returns FALSE and prevents movement if the object we're pulling is too far and the direction
  * src is moving isn't towards the pulled object.
  * Returns TRUE and allows movement if the object we're pulling is in range.
  */
/atom/movable/proc/handle_pulled_premove(atom/newloc, direct, _step_x, _step_y)
	if((bounds_dist(src, pulling) > 8 + maxspeed) && !(direct & GET_PIXELDIR(src, pulling)))
		return FALSE
	return TRUE

/atom/movable/Move(atom/newloc, direct, _step_x, _step_y)
	if(SEND_SIGNAL(src, COMSIG_MOVABLE_PRE_MOVE, newloc, direct, _step_x, _step_y) & COMPONENT_MOVABLE_BLOCK_PRE_MOVE)
		return FALSE

	if(!premove_pull_checks(newloc, direct, _step_x, _step_y))
		return FALSE
	var/atom/oldloc = loc

	. = ..()
	if(!. && can_sidestep && !sidestep)
		//if this mob is able to slide when colliding, and is currently not attempting to slide
		//mark that we are sliding
		sidestep = TRUE
		//call to the slider object to determine what direction our slide will happen in (if any)
		. = slider.slide(src, direct, _step_x, _step_y)
		if(.)
			//if slider was able to slide, step us in the direction indicated
			. = step(src, ., 2)
		//mark that we are no longer sliding
		sidestep = FALSE
	last_move = direct
	setDir(direct)
	if(.)
		Moved(oldloc, direct)
		if(pulling) //we were pulling a thing and didn't lose it during our move.
			handle_pulled_movement()
			check_pulling()
		if(has_buckled_mobs() && !handle_buckled_mob_movement(loc, direct, step_x, step_y))
			return FALSE
	else // we still didn't move, something is blocking further movement
		walk(src, NONE)

///Handles premove checks, called on client.mob by client/Move as well
/atom/movable/proc/premove_pull_checks(newloc, direct, _step_x, _step_y)
	if(pulling && !handle_pulled_premove(newloc, direct, _step_x, _step_y))
		handle_pulled_movement()
		check_pulling()
		return FALSE
	return TRUE

/**
  * Adds velocity to an movable atom
  *
  * Starts processing on SSmovement if the movable was not previously moving
  * handles math for acceleration and diagonals, prevents further acceleration if maxspeed is reached
  * Arguments:
  * * direct - The direction of velocity
  * * acceleration - if null, uses movables accel var. Otherwise overwrites the accel var to acceleration by a set amount
  * * force - defaults to FALSE, if TRUE ignores maxspeed and forces acceleration to be added to velocity
  */
/atom/movable/proc/add_velocity(direct = 0, acceleration = null, force = FALSE)
	if(vx == 0 && vy == 0)
		SSmovement.moving[src] = src
	var/accelu = accel
	if(!isnull(acceleration)) // acceleration override
		accelu = acceleration
	var/limit_speed = 0
	if(!force || vx * vx + vy * vy <= maxspeed * maxspeed + 1)
		limit_speed = 1
	if(direct & EAST)
		if(vx < maxspeed || force)
			vx += accelu
	else if(direct & WEST)
		if(vx > -maxspeed || force)
			vx -= accelu
	if(direct & NORTH)
		if(vy < maxspeed || force)
			vy += accelu
	else if(direct & SOUTH)
		if(vy > -maxspeed || force)
			vy -= accelu
	if(limit_speed)
		var/len = sqrt(vx * vx + vy * vy)
		if(len > maxspeed)
			vx = round(maxspeed * vx / len, 0.1)
			vy = round(maxspeed * vy / len, 0.1)
	if(vx != 0 || vy != 0) // we have velocity
		vdir = direct
		return TRUE // test the waters

/**
  * Forces the velocity of a movable atom to the value defined in velocity
  *
  * Starts processing on SSmovement if the movable was not previously moving
  * handles math for acceleration and diagonals, prevents further acceleration if maxspeed is reached
  * Arguments:
  * * direct - The direction of velocity
  * * velocity - The velocity to be forced in the direction provided
  */
/atom/movable/proc/force_velocity(direct, velocity)
	if(vx == 0 && vy == 0)
		SSmovement.moving[src] = src
	if(direct & EAST)
		vx = velocity
	else if(direct & WEST)
		vx = -velocity
	if(direct & NORTH)
		vy = velocity
	else if(direct & SOUTH)
		vy = -velocity

	if(vx != 0 || vy != 0) // we have velocity
		vdir = direct
		return TRUE

/atom/movable/proc/handle_inertia()
	var/move_x = vx
	var/move_y = vy
	// find the integer part of your move
	var/ipx = round(abs(vx)) * ((vx < 0) ? -1 : 1)
	var/ipy = round(abs(vy)) * ((vy < 0) ? -1 : 1)

	// accumulate the fractional parts of the move
	fx += (move_x - ipx)
	fy += (move_y - ipy)

	// ignore the fractional parts
	move_x = ipx
	move_y = ipy

	// increment the move if the fractions have added up
	while(fx > 0.5)
		fx -= 0.5
		move_x += 1
	while(fx < -0.5)
		fx += 0.5
		move_x -= 1

	while(fy > 0.5)
		fy -= 0.5
		move_y += 1
	while(fy < -0.5)
		fy += 0.5
		move_y -= 1
	var/old_step_size = step_size
	// Enable sliding to anywhere in the world.
	step_size = 1#INF
	//change dir
	var/dir_to_set
	if(move_x > 0)
		dir_to_set = EAST
	else if(move_x < 0)
		dir_to_set = WEST
	if(move_y > 0)
		dir_to_set |= NORTH
	else if(move_y < 0)
		dir_to_set |= SOUTH
	// Move and return the result.
	. = Move(loc, dir_to_set, step_x + move_x, step_y + move_y)
	if(!.) // movement failed, means we can't move either
		vx = 0
		vy = 0
		vdir = NONE
		step_size = old_step_size
		return FALSE
	// Set step_size to the amount actually moved.
	// This tells clients to smoothly interpolate this when using client.fps.
	step_size = 1 + .

	if(vx > maxspeed)
		vx -= decel
	else if(vx < -maxspeed)
		vx += decel
	if(vy > maxspeed)
		vy -= decel
	else if(vy < -maxspeed)
		vy += decel
	// begin decelerating if movement keys aren't held
	if(ismob(src))
		var/mob/M = src
		if(M.client)
			var/client/user = M.client
			var/movement_dir = NONE
			for(var/_key in user.keys_held)
				movement_dir = movement_dir | user.movement_keys[_key]
			if(!(movement_dir & EAST || movement_dir & WEST))
				if(vx > decel)
					vx -= decel
				else if(vx < -decel)
					vx += decel
				else
					vx = 0
			if(!(movement_dir & NORTH || movement_dir & SOUTH))
				if(vy > decel)
					vy -= decel
				else if(vy < -decel)
					vy += decel
				else
					vy = 0
		else if(vdir) // copy pasta for mobs without clients
			if((vdir & EAST) || (vdir & WEST))
				if(vx > decel)
					vx -= decel
				else if(vx < -decel)
					vx += decel
				else
					vx = 0

			if((vdir & SOUTH) || (vdir & NORTH))
				if(vy > decel)
					vy -= decel
				else if(vy < -decel)
					vy += decel
				else
					vy = 0
	else if(vdir) // for movables
		if((vdir & EAST) || (vdir & WEST))
			if(vx > decel)
				vx -= decel
			else if(vx < -decel)
				vx += decel
			else
				vx = 0

		if((vdir & SOUTH) || (vdir & NORTH))
			if(vy > decel)
				vy -= decel
			else if(vy < -decel)
				vy += decel
			else
				vy = 0
	//update vdir
	if(vx > 0)
		vdir = EAST
	else if(vx < 0)
		vdir = WEST
	if(vy > 0)
		vdir |= NORTH
	else if(vy < 0)
		vdir |= SOUTH
	else if(vx == 0 && vy == 0) // vx and vy is 0 we have no inertia
		vdir = NONE
		return FALSE

/**
 * meant for movement with zero side effects. only use for objects that are supposed to move "invisibly" (like camera mobs or ghosts)
 * if you want something to move onto a tile with a beartrap or recycler or tripmine or mouse without that object knowing about it at all, use this
 * most of the time you want forceMove()
 */
/atom/movable/proc/abstract_move(atom/new_loc)
	var/atom/old_loc = loc
	move_stacks++
	loc = new_loc
	Moved(old_loc)

/**
 * * Called after a successful Move(). By this point, we've already moved * Arguments:
 * * old_loc is the location prior to the move. Can be null to indicate nullspace.
 * * movement_dir is the direction the movement took place. Can be NONE if it was some sort of teleport.
 * * The forced flag indicates whether this was a forced move, which skips many checks of regular movement.
 * * The old_locs is an optional argument, in case the moved movable was present in multiple locations before the movement.
 **/
/atom/movable/proc/Moved(atom/old_loc, movement_dir, forced = FALSE, list/old_locs)
	SHOULD_CALL_PARENT(TRUE)
	SEND_SIGNAL(src, COMSIG_MOVABLE_MOVED, old_loc, movement_dir, forced, old_locs)

	if(old_loc == loc)
		var/turf/old_turf = get_turf(old_loc)
		var/turf/new_turf = get_turf(src)
		if(old_turf && new_turf && old_turf.z != new_turf.z)
			onTransitZ(old_turf.z, new_turf.z)
	else
		SEND_SIGNAL(src, COMSIG_MOVABLE_MOVED_TURF, old_loc, movement_dir)
	if (!inertia_moving)
		newtonian_move(movement_dir)

	// Z-Mimic hook
	if (bound_overlay)
		// The overlay will handle cleaning itself up on non-openspace turfs.
		if (isturf(loc))
			bound_overlay.forceMove(get_step(src, UP))
			if (bound_overlay && dir != bound_overlay.dir)
				bound_overlay.setDir(dir)
		else	// Not a turf, so we need to destroy immediately instead of waiting for the destruction timer to proc.
			qdel(bound_overlay)

	var/turf/old_turf = get_turf(old_loc)
	var/turf/new_turf = get_turf(src)

	if(HAS_SPATIAL_GRID_CONTENTS(src))
		if(old_turf && new_turf && (old_turf.z != new_turf.z \
			|| ROUND_UP(old_turf.x / SPATIAL_GRID_CELLSIZE) != ROUND_UP(new_turf.x / SPATIAL_GRID_CELLSIZE) \
			|| ROUND_UP(old_turf.y / SPATIAL_GRID_CELLSIZE) != ROUND_UP(new_turf.y / SPATIAL_GRID_CELLSIZE)))

			SSspatial_grid.exit_cell(src, old_turf)
			SSspatial_grid.enter_cell(src, new_turf)

		else if(old_turf && !new_turf)
			SSspatial_grid.exit_cell(src, old_turf)

		else if(new_turf && !old_turf)
			SSspatial_grid.enter_cell(src, new_turf)

	return TRUE

// Make sure you know what you're doing if you call this, this is intended to only be called by byond directly.
// You probably want CanPass()
/atom/movable/Cross(atom/movable/AM)
	SEND_SIGNAL(src, COMSIG_MOVABLE_CROSS, AM)
	SEND_SIGNAL(AM, COMSIG_MOVABLE_CROSS_OVER, src)
	return CanPass(AM, get_dir(src, AM), TRUE)

///default byond proc that is deprecated for us in lieu of signals. do not call
/atom/movable/Crossed(atom/movable/AM, oldloc)
	SHOULD_NOT_OVERRIDE(TRUE)
	CRASH("atom/movable/Crossed() was called!")

/**
 * `Uncross()` is a default BYOND proc that is called when something is *going*
 * to exit this atom's turf. It is preferred over `Uncrossed` when you want to
 * deny that movement, such as in the case of border objects, objects that allow
 * you to walk through them in any direction except the one they block
 * (think side windows).
 *
 * While being seemingly harmless, most everything doesn't actually want to
 * use this, meaning that we are wasting proc calls for every single atom
 * on a turf, every single time something exits it, when basically nothing
 * cares.
 *
 * This overhead caused real problems on Sybil round #159709, where lag
 * attributed to Uncross was so bad that the entire master controller
 * collapsed and people made Among Us lobbies in OOC.
 *
 * If you want to replicate the old `Uncross()` behavior, the most apt
 * replacement is [`/datum/element/connect_loc`] while hooking onto
 * [`COMSIG_ATOM_EXIT`].
 */
/atom/movable/Uncross()
	SHOULD_NOT_OVERRIDE(TRUE)
	CRASH("Uncross() should not be being called, please read the doc-comment for it for why.")

/**
 * default byond proc that is normally called on everything inside the previous turf
 * a movable was in after moving to its current turf
 * this is wasteful since the vast majority of objects do not use Uncrossed
 * use connect_loc to register to COMSIG_ATOM_EXITED instead
 */
/atom/movable/Uncrossed(atom/movable/AM)
	SHOULD_NOT_OVERRIDE(TRUE)
	CRASH("/atom/movable/Uncrossed() was called")

/atom/movable/Bump(atom/A)
	SEND_SIGNAL(src, COMSIG_MOVABLE_BUMP, A)
	. = ..()
	if(!QDELETED(throwing))
		throwing.finalize(hit = TRUE, target = A)
		. = TRUE
		if(QDELETED(A))
			return
	A.Bumped(src)

/atom/movable/setDir(direct)
	var/old_dir = dir
	. = ..()
	update_bounds(olddir=old_dir, newdir=direct)

/atom/movable/true_x()
	. = ..()
	. += step_x

/atom/movable/true_y()
	. = ..()
	. += step_y

/**
  * Updates bounds of the object depending on its brotation define
  *
  * Called on setDir and updates the bounds accordingly
  * Unless you have some really weird rotation try to implement a generic version of your rotation here and make a flag for it
  * Arguments:
  * * olddir - The old direction
  * * newdir - The new direction
  */
/atom/movable/proc/update_bounds(olddir, newdir)
	SEND_SIGNAL(src, COMSIG_MOVABLE_UPDATE_BOUNDS, args)

	if(newdir == olddir) // the direction hasn't changed
		return
	if(bound_width == bound_height && !bound_x && !bound_y) // We're a square and have no offset
		return

	if(brotation & BOUNDS_SIMPLE_ROTATE)
		var/rot = SIMPLIFY_DEGREES(dir2angle(newdir)-dir2angle(olddir))
		for(var/i in 90 to rot step 90)
			var/tempwidth = bound_width
			var/eastgap = CEILING(bound_width, 32) - bound_width - bound_x

			bound_width = bound_height
			bound_height = tempwidth

			bound_x = bound_y
			bound_y = eastgap

///Sets the anchored var and returns if it was sucessfully changed or not.
/atom/movable/proc/set_anchored(anchorvalue)
	SHOULD_CALL_PARENT(TRUE)
	if(anchored == anchorvalue)
		return
	. = anchored
	set_sidestep(!anchored) // can't sidestep when anchored, can sidestep when not anchored
	anchored = anchorvalue
	set_sidestep(!anchored)
	SEND_SIGNAL(src, COMSIG_MOVABLE_SET_ANCHORED, anchorvalue)

/atom/movable/proc/forceMove(atom/destination, _step_x=0, _step_y=0)
	. = FALSE
	if(islist(destination))
		if(length(destination) > 2)
			_step_x  = destination[2]
			_step_y = destination[3]
		destination = get_turf(destination[1])
	if(destination)
		. = doMove(destination, _step_x, _step_y)
	else
		CRASH("No valid destination passed into forceMove")

/// sets the step_ offsets to AM, or if AM is null sets the step_ values to the offsets
/**
  * sets the step_ offsets to that of AM, or if AM is null sets the step_ values to the offsets
  *
  * Pixel counterpart of forceMove
  * should be used when only wanting to set the step values
  * can either pass a movable you want to copy step values from
  * leave AM null and input step_ values manually
  * Arguments:
  * * AM - The movable we want to copy step_ values from
  * * _step_x - Alternative step_x value when AM is null
  * * _step_y - Alternative step_y value when AM is null
  */
/atom/movable/proc/forceStep(atom/movable/AM=null, _step_x=0, _step_y=0)
	if(!AM)
		step_x = _step_x
		step_y = _step_y
	else
		step_x = AM.step_x
		step_y = AM.step_y

/atom/movable/proc/moveToNullspace()
	return doMove(null)

/atom/movable/proc/doMove(atom/destination, _step_x=0, _step_y=0)
	. = FALSE
	if(destination == loc) // Force move in place?
		Moved(loc, NONE, TRUE)
		return TRUE

	var/atom/oldloc = loc
	var/area/oldarea = get_area(oldloc)
	var/area/destarea = get_area(destination)
	var/list/old_bounds = obounds()

	loc = destination
	step_x = _step_x
	step_y = _step_y

	if(oldloc && oldloc != loc)
		oldloc.Exited(src, destination)
		if(oldarea && oldarea != destarea)
			oldarea.Exited(src, destination)

	var/list/new_bounds = obounds()

	for(var/i in old_bounds)
		if(i in new_bounds)
			continue
		var/atom/thing = i
		thing.Uncrossed(src)

	if(pulledby || pulling)
		check_pulling()

	if(!loc) // I hope you know what you're doing
		return TRUE

	var/turf/oldturf = get_turf(oldloc)
	var/turf/destturf = get_turf(destination)
	var/oldz = (oldturf ? oldturf.z : null)
	var/newz = (destturf ? destturf.z : null)
	if(oldz != newz)
		onTransitZ(oldz, newz)

	destination.Entered(src, oldloc)
	if(destarea && oldarea != destarea)
		destarea.Entered(src, oldloc)

	for(var/i in new_bounds)
		if(i in old_bounds)
			continue
		var/atom/thing = i
		thing.Crossed(src, oldloc)

	Moved(oldloc, NONE, TRUE)
	return TRUE

//Called whenever an object moves and by mobs when they attempt to move themselves through space
//And when an object or action applies a force on src, see newtonian_move() below
//Return 0 to have src start/keep drifting in a no-grav area and 1 to stop/not start drifting
//Mobs should return 1 if they should be able to move of their own volition, see client/Move() in mob_movement.dm
//movement_dir == 0 when stopping or any dir when trying to move
/atom/movable/proc/Process_Spacemove(movement_dir = FALSE)
	if(has_gravity(src))
		return TRUE

	if(pulledby && (pulledby.pulledby != src || moving_from_pull))
		return TRUE

	if(throwing)
		return TRUE

	if(!isturf(loc))
		return TRUE

	if(locate(/obj/structure/lattice) in range(1, get_turf(src))) //Not realistic but makes pushing things in space easier
		return TRUE

	return FALSE


/// Only moves the object if it's under no gravity
/// Accepts the direction to move, and if the push should be instant
/atom/movable/proc/newtonian_move(direction, instant = FALSE)
	if(!isturf(loc) || Process_Spacemove(0) || !direction)
		return FALSE

	if(SEND_SIGNAL(src, COMSIG_MOVABLE_NEWTONIAN_MOVE, direction) & COMPONENT_MOVABLE_NEWTONIAN_BLOCK)
		return TRUE

	set_glide_size(MOVEMENT_ADJUSTED_GLIDE_SIZE(inertia_move_delay, SSspacedrift.visual_delay))
	AddComponent(/datum/component/drift, direction, instant)

	return TRUE

/atom/movable/proc/throw_impact(atom/hit_atom, datum/thrownthing/throwingdatum)
	set waitfor = 0
	SEND_SIGNAL(src, COMSIG_MOVABLE_IMPACT, hit_atom, throwingdatum)
	return hit_atom.hitby(src, throwingdatum=throwingdatum)

/atom/movable/hitby(atom/movable/AM, skipcatch, hitpush = TRUE, blocked, datum/thrownthing/throwingdatum)
	if(!anchored && hitpush && (!throwingdatum || (throwingdatum.force >= (move_resist * MOVE_FORCE_PUSH_RATIO))))
		step(src, AM.dir, 16)
	..(AM, skipcatch, hitpush, blocked, throwingdatum)

/atom/movable/proc/safe_throw_at(atom/target, range, speed, mob/thrower, spin = TRUE, diagonals_first = FALSE, datum/callback/callback, force = MOVE_FORCE_STRONG, params)
	if((force < (move_resist * MOVE_FORCE_THROW_RATIO)) || (move_resist == INFINITY))
		return
	return throw_at(target, range, speed, thrower, spin, diagonals_first, callback, force, TRUE, params)

/atom/movable/proc/throw_at(atom/target, range, speed, mob/thrower, spin = TRUE, diagonals_first = FALSE, datum/callback/callback, force = MOVE_FORCE_STRONG, quickstart = TRUE, params) //If this returns FALSE then callback will not be called.
	. = FALSE

	if(QDELETED(src))
		CRASH("Qdeleted thing being thrown around.")

	//Snowflake case for click masks
	if (istype(target, /atom/movable/screen))
		target = target.loc

	if (!target || speed <= 0)
		return

	if(SEND_SIGNAL(src, COMSIG_MOVABLE_PRE_THROW, args) & COMPONENT_CANCEL_THROW)
		return

	if (pulledby)
		pulledby.stop_pulling()


	//They are moving! Wouldn't it be cool if we calculated their momentum and added it to the throw?
	if (thrower && thrower.last_move && thrower.client && thrower.client.move_delay >= world.time + world.tick_lag*2)
		var/user_momentum = thrower.cached_multiplicative_slowdown
		if (!user_momentum) //no movement_delay, this means they move once per byond tick, lets calculate from that instead.
			user_momentum = world.tick_lag

		user_momentum = 1 / user_momentum // convert from ds to the tiles per ds that throw_at uses.

		if (get_dir(thrower, target) & last_move)
			user_momentum = user_momentum //basically a noop, but needed
		else if (get_dir(target, thrower) & last_move)
			user_momentum = -user_momentum //we are moving away from the target, lets slowdown the throw accordingly
		else
			user_momentum = 0


		if (user_momentum)
			//first lets add that momentum to range.
			range *= (user_momentum / speed) + 1
			//then lets add it to speed
			speed += user_momentum
			if (speed <= 0)
				return//no throw speed, the user was moving too fast.

	. = TRUE // No failure conditions past this point.

	var/target_zone
	if(QDELETED(thrower) || !istype(thrower))
		thrower = null //Let's not pass an invalid reference.
	else
		target_zone = thrower.get_combat_bodyzone(target)

	var/datum/thrownthing/TT = new(src, target, get_dir(src, target), range, speed, thrower, diagonals_first, force, callback, target_zone)
	if(thrower && params)
		var/list/calculated = calculate_projectile_angle_and_pixel_offsets(thrower, params)
		TT.angle = calculated[1]
	var/dist_x = abs(target.x - src.x)
	var/dist_y = abs(target.y - src.y)
	var/dx = (target.x > src.x) ? EAST : WEST
	var/dy = (target.y > src.y) ? NORTH : SOUTH

	if (dist_x == dist_y)
		TT.pure_diagonal = 1

	else if(dist_x <= dist_y)
		var/olddist_x = dist_x
		var/olddx = dx
		dist_x = dist_y
		dist_y = olddist_x
		dx = dy
		dy = olddx
	TT.dist_x = dist_x
	TT.dist_y = dist_y
	TT.dx = dx
	TT.dy = dy
	TT.diagonal_error = dist_x/2 - dist_y
	TT.start_time = world.time

	if(pulledby)
		pulledby.stop_pulling()
	movement_type |= THROWN

	throwing = TT
	if(TT.hitcheck())
		return
	if(spin)
		SpinAnimation(5, 1)

	SEND_SIGNAL(src, COMSIG_MOVABLE_POST_THROW, TT, spin)
	SSthrowing.processing[src] = TT
	if (SSthrowing.state == SS_PAUSED && length(SSthrowing.currentrun))
		SSthrowing.currentrun[src] = TT

	if(quickstart)
		TT.tick()

/atom/movable/proc/handle_buckled_mob_movement(newloc, direct, _step_x, _step_y)
	for(var/m in buckled_mobs)
		var/mob/living/buckled_mob = m
		if(!buckled_mob.Move(newloc, direct, _step_x, _step_y))
			doMove(buckled_mob.loc, step_x, step_y) //forceMove breaks buckles on stairs, use doMove
			last_move = buckled_mob.last_move
			last_move_time = world.time
			return FALSE
	return TRUE

/atom/movable/proc/force_pushed(atom/movable/pusher, force = MOVE_FORCE_DEFAULT, direction)
	return FALSE

/atom/movable/proc/force_push(atom/movable/AM, force = move_force, direction, silent = FALSE)
	. = AM.force_pushed(src, force, direction)
	if(!silent && .)
		visible_message(span_warning("[src] forcefully pushes against [AM]!"), span_warning("You forcefully push against [AM]!"))

/atom/movable/proc/move_crush(atom/movable/AM, force = move_force, direction, silent = FALSE)
	. = AM.move_crushed(src, force, direction)
	if(!silent && .)
		visible_message(span_danger("[src] crushes past [AM]!"), span_danger("You crush [AM]!"))

/atom/movable/proc/move_crushed(atom/movable/pusher, force = MOVE_FORCE_DEFAULT, direction)
	return FALSE

/atom/movable/CanAllowThrough(atom/movable/mover, border_dir)
	. = ..()
	if(mover in buckled_mobs)
		return TRUE

/// Returns true or false to allow src to move through the blocker, mover has final say
/atom/movable/proc/CanPassThrough(atom/blocker, movement_dir, blocker_opinion)
	SHOULD_CALL_PARENT(TRUE)
	return blocker_opinion

// called when this atom is removed from a storage item, which is passed on as S. The loc variable is already set to the new destination before this is called.
/atom/movable/proc/on_exit_storage(datum/component/storage/concrete/S)
	return

// called when this atom is added into a storage item, which is passed on as S. The loc variable is already set to the storage item.
/atom/movable/proc/on_enter_storage(datum/component/storage/concrete/S)
	return

/atom/movable/proc/get_spacemove_backup()
	for(var/A in obounds(src, 16))
		if(isarea(A))
			continue
		else if(isturf(A))
			var/turf/turf = A
			if(!turf.density)
				continue
			return turf
		else
			var/atom/movable/AM = A
			if(AM.density || !AM.CanPass(src, get_dir(src, AM)))
				if(AM.inertia_dir)
					continue
				return AM

//Called when something resists while this atom is its loc
/atom/movable/proc/container_resist(mob/living/user)
	return

//Called when a mob resists while inside a container that is itself inside something.
/atom/movable/proc/relay_container_resist(mob/living/user, obj/O)
	return

/atom/movable/proc/do_attack_animation(atom/A, visual_effect_icon, obj/item/used_item, no_effect)
	if(!no_effect && (visual_effect_icon || used_item))
		do_item_attack_animation(A, visual_effect_icon, used_item)

	if(A == src)
		return //don't do an animation if attacking self
	var/pixel_x_diff = 0
	var/pixel_y_diff = 0
	var/turn_dir = 1

	var/direction = get_dir(src, A)
	if(direction & NORTH)
		pixel_y_diff = 8
		turn_dir = prob(50) ? -1 : 1
	else if(direction & SOUTH)
		pixel_y_diff = -8
		turn_dir = prob(50) ? -1 : 1

	if(direction & EAST)
		pixel_x_diff = 8
	else if(direction & WEST)
		pixel_x_diff = -8

	var/matrix/initial_transform = matrix(transform)
	var/matrix/rotated_transform = transform.Turn(rand(13,17) * turn_dir)
	animate(src, pixel_x = pixel_x + pixel_x_diff, pixel_y = pixel_y + pixel_y_diff, transform=rotated_transform, time = 1, easing=BACK_EASING|EASE_IN, flags = ANIMATION_PARALLEL)
	animate(pixel_x = pixel_x - pixel_x_diff, pixel_y = pixel_y - pixel_y_diff, transform=initial_transform, time = 2, easing=SINE_EASING, flags = ANIMATION_PARALLEL)

/atom/movable/proc/do_item_attack_animation(atom/A, visual_effect_icon, obj/item/used_item)
	var/image/I
	var/obj/effect/icon/temp/attack_animation_object
	if(visual_effect_icon)
		I = image('icons/effects/effects.dmi', A, visual_effect_icon, A.layer + 0.1)
		attack_animation_object = new(get_turf(A), I, 10) //A.loc is an area when A is a turf
	else if(used_item)
		I = image(icon = used_item, loc = A, layer = A.layer + 0.1)
		I.plane = GAME_PLANE
		I.appearance_flags = NO_CLIENT_COLOR | PIXEL_SCALE
		attack_animation_object = new(get_turf(A), I, 10)

		// Scale the icon.
		attack_animation_object.transform *= pick(0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55)

		// Set the direction of the icon animation.
		var/direction = get_dir(src, A)
		if(direction & NORTH)
			attack_animation_object.pixel_y = rand(-15,-11)
		else if(direction & SOUTH)
			attack_animation_object.pixel_y = rand(11,15)

		if(direction & EAST)
			attack_animation_object.pixel_x = rand(-15,-11)
		else if(direction & WEST)
			attack_animation_object.pixel_x = rand(11,15)

		if(!direction) // Attacked self?!
			attack_animation_object.pixel_z = 16

	if(!I)
		return

	// And animate the attack!
	animate(attack_animation_object, alpha = 175, transform = matrix() * 0.75, pixel_x = 0, pixel_y = 0, pixel_z = 0, time = 3)
	animate(time = 1)
	animate(alpha = 0, time = 3, easing = CIRCULAR_EASING|EASE_OUT)

/atom/movable/vv_get_dropdown()
	. = ..()
	. += "<option value='?_src_=holder;[HrefToken()];adminplayerobservefollow=[REF(src)]'>Follow</option>"
	. += "<option value='?_src_=holder;[HrefToken()];admingetmovable=[REF(src)]'>Get</option>"

	VV_DROPDOWN_OPTION(VV_HK_EDIT_PARTICLES, "Edit Particles")
	VV_DROPDOWN_OPTION(VV_HK_ADD_EMITTER, "Add Emitter")
	VV_DROPDOWN_OPTION(VV_HK_REMOVE_EMITTER, "Remove Emitter")

/atom/movable/vv_do_topic(list/href_list)
	. = ..()
	if(href_list[VV_HK_EDIT_PARTICLES])
		if(!check_rights(R_VAREDIT))
			return
		var/client/interacted_client = usr.client
		interacted_client?.open_particle_editor(src)


	if(href_list[VV_HK_ADD_EMITTER])
		if(!check_rights(R_VAREDIT))
			return

		var/key = stripped_input(usr, "Enter a key for your emitter", "Emitter Key")
		var/lifetime = input("How long should this live for in deciseconds? 0 for infinite, -1 for a single burst.", "Lifespan") as null|num

		if(!key)
			return
		switch(alert("Should this be a pre-filled emitter (empty emitters don't support timers)?",,"Yes","No","Cancel"))
			if("Yes")
				var/choice = input(usr, "Choose an emitter to add", "Choose an Emitter") as null|anything in subtypesof(/obj/emitter)
				var/should_burst = FALSE
				if(lifetime == -1)
					should_burst = TRUE
				if(choice)
					add_emitter(choice, key, lifespan = lifetime, burst_mode = should_burst)
			if("No")
				add_emitter(/obj/emitter, key)
			else
				return

	if(href_list[VV_HK_REMOVE_EMITTER])
		if(!check_rights(R_VAREDIT))
			return
		if(!master_holder?.emitters.len)
			return
		var/removee = input(usr, "Choose an emitter to remove", "Choose an Emitter") as null|anything in master_holder?.emitters
		if(!removee)
			return
		remove_emitter(removee)

/atom/movable/proc/ex_check(ex_id)
	if(!ex_id)
		return TRUE
	LAZYINITLIST(acted_explosions)
	if(ex_id in acted_explosions)
		return FALSE
	acted_explosions += ex_id
	return TRUE

/* 	Language procs
*	Unless you are doing something very specific, these are the ones you want to use.
*/

/// Gets or creates the relevant language holder. For mindless atoms, gets the local one. For atom with mind, gets the mind one.
/atom/movable/proc/get_language_holder()
	RETURN_TYPE(/datum/language_holder)
	if(QDELING(src))
		CRASH("get_language_holder() called on a QDELing atom, \
			this will try to re-instantiate the language holder that's about to be deleted, which is bad.")

	if(!language_holder)
		language_holder = new initial_language_holder(src)
	return language_holder

/// Grants the supplied language and sets omnitongue true.
/atom/movable/proc/grant_language(language, language_flags = ALL, source = LANGUAGE_ATOM)
	return get_language_holder().grant_language(language, language_flags, source)

/// Grants every language.
/atom/movable/proc/grant_all_languages(language_flags = ALL, grant_omnitongue = TRUE, source = LANGUAGE_MIND)
	return get_language_holder().grant_all_languages(language_flags, grant_omnitongue, source)

/// Removes a single language.
/atom/movable/proc/remove_language(language, language_flags = ALL, source = LANGUAGE_ALL)
	return get_language_holder().remove_language(language, language_flags, source)

/// Removes every language and sets omnitongue false.
/atom/movable/proc/remove_all_languages(source = LANGUAGE_ALL, remove_omnitongue = FALSE)
	return get_language_holder().remove_all_languages(source, remove_omnitongue)

/// Adds a language to the blocked language list. Use this over remove_language in cases where you will give languages back later.
/atom/movable/proc/add_blocked_language(language, source = LANGUAGE_ATOM)
	return get_language_holder().add_blocked_language(language, source)

/// Removes a language from the blocked language list.
/atom/movable/proc/remove_blocked_language(language, source = LANGUAGE_ATOM)
	return get_language_holder().remove_blocked_language(language, source)

/// Checks if atom has the language. If spoken is true, only checks if atom can speak the language.
/atom/movable/proc/has_language(language, flags_to_check)
	return get_language_holder().has_language(language, flags_to_check)

/// Checks if atom can speak the language.
/atom/movable/proc/can_speak_language(language)
	return get_language_holder().can_speak_language(language)

/// Returns the result of tongue specific limitations on spoken languages.
/atom/movable/proc/could_speak_language(datum/language/language_path)
	return TRUE

/// Returns selected language, if it can be spoken, or finds, sets and returns a new selected language if possible.
/atom/movable/proc/get_selected_language()
	return get_language_holder().get_selected_language()

/// Gets a random understood language, useful for hallucinations and such.
/atom/movable/proc/get_random_understood_language()
	return get_language_holder().get_random_understood_language()

/// Gets a random spoken language, useful for forced speech and such.
/atom/movable/proc/get_random_spoken_language()
	return get_language_holder().get_random_spoken_language()

/// Copies all languages into the supplied atom/language holder. Source should be overridden when you
/// do not want the language overwritten by later atom updates or want to avoid blocked languages.
/atom/movable/proc/copy_languages(datum/language_holder/from_holder, source_override=FALSE, spoken=TRUE, understood=TRUE, blocked=TRUE)
	if(ismovable(from_holder))
		var/atom/movable/thing = from_holder
		from_holder = thing.get_language_holder()

	return get_language_holder().copy_languages(from_holder, source_override, spoken, understood, blocked)

/// Sets the passed path as the active language
/// Returns the currently selected language if successful, if the language was not valid, returns null
/atom/movable/proc/set_active_language(language_path)
	var/datum/language_holder/our_holder = get_language_holder()
	our_holder.selected_language = language_path

	return our_holder.get_selected_language() // verifies its validity, returns it if successful.

/* End language procs */

/atom/movable/drop_location()
	return list(get_turf(src), step_x, step_y)

//Returns an atom's power cell, if it has one. Overload for individual items.
/atom/movable/proc/get_cell()
	return

/atom/movable/proc/can_be_pulled(user, grab_state, force)
	if(src == user || !isturf(loc))
		return FALSE
	if(anchored || throwing)
		return FALSE
	if(force < (move_resist * MOVE_FORCE_PULL_RATIO))
		return FALSE
	return TRUE

/// Updates the grab state of the movable
/// This exists to act as a hook for behaviour
/atom/movable/proc/setGrabState(newstate)
	if(newstate == grab_state)
		return
	SEND_SIGNAL(src, COMSIG_MOVABLE_SET_GRAB_STATE, newstate)
	. = grab_state
	grab_state = newstate
	switch(.) //Previous state.
		if(GRAB_PASSIVE, GRAB_AGGRESSIVE)
			if(grab_state >= GRAB_NECK)
				ADD_TRAIT(pulling, TRAIT_IMMOBILIZED, CHOKEHOLD_TRAIT)
				ADD_TRAIT(pulling, TRAIT_FLOORED, CHOKEHOLD_TRAIT)
				ADD_TRAIT(pulling, TRAIT_HANDS_BLOCKED, CHOKEHOLD_TRAIT)
	switch(grab_state) //Current state.
		if(GRAB_PASSIVE, GRAB_AGGRESSIVE)
			if(. >= GRAB_NECK)
				REMOVE_TRAIT(pulling, TRAIT_IMMOBILIZED, CHOKEHOLD_TRAIT)
				REMOVE_TRAIT(pulling, TRAIT_FLOORED, CHOKEHOLD_TRAIT)
				REMOVE_TRAIT(pulling, TRAIT_HANDS_BLOCKED, CHOKEHOLD_TRAIT)

/obj/item/proc/do_pickup_animation(atom/target)
	set waitfor = FALSE
	if(!istype(loc, /turf))
		return
	var/image/pickup_animation = image(icon = src, loc = loc, layer = layer + 0.1)
	pickup_animation.plane = GAME_PLANE
	pickup_animation.appearance_flags = NO_CLIENT_COLOR | PIXEL_SCALE
	var/turf/current_turf = get_turf(src)
	var/direction = get_dir(current_turf, target)
	var/to_x = target.base_pixel_x
	var/to_y = target.base_pixel_y

	if(direction & NORTH)
		to_y += 32
	else if(direction & SOUTH)
		to_y -= 32
	if(direction & EAST)
		to_x += 32
	else if(direction & WEST)
		to_x -= 32
	if(!direction)
		to_y += 10
		pickup_animation.pixel_x += 6 * (prob(50) ? 1 : -1) //6 to the right or left, helps break up the straight upward move

	var/obj/effect/icon/temp/pickup_animation_object = new(loc, pickup_animation, 4)
	pickup_animation_object.transform *= 0.75
	var/matrix/animation_matrix = new(pickup_animation_object.transform)
	animation_matrix.Turn(pick(-30, 30))
	animation_matrix.Scale(0.65)

	animate(pickup_animation_object, alpha = 175, pixel_x = to_x, pixel_y = to_y, time = 3, transform = animation_matrix, easing = CUBIC_EASING)
	animate(alpha = 0, transform = matrix().Scale(0.7), time = 1)

/obj/item/proc/do_drop_animation(atom/moving_from)
	set waitfor = FALSE
	if(item_flags & WAS_THROWN)
		return
	if(movement_type & THROWN)
		return
	if(!istype(loc, /turf))
		return
	var/stepx = 0
	var/stepy = 0
	if(ismovable(target))
		var/atom/movable/AM = target
		stepx = AM.step_x
		stepy = AM.step_y
	var/turf/current_turf = get_turf(src)
	var/direction = get_dir(moving_from, current_turf)
	var/from_x = moving_from.base_pixel_x
	var/from_y = moving_from.base_pixel_y

	if(direction & NORTH)
		from_y -= 32 + stepy
	else if(direction & SOUTH)
		from_y += 32 - stepy
	if(direction & EAST)
		from_x -= 32 + stepx
	else if(direction & WEST)
		from_x += 32 - stepx
	if(!direction)
		if(!(stepx || stepy))
			to_y = 8
		else
			to_x = stepx
			to_y = stepy

	//We're moving from these chords to our current ones
	var/old_x = pixel_x
	var/old_y = pixel_y
	var/old_alpha = alpha
	var/matrix/old_transform = transform
	var/matrix/animation_matrix = new(old_transform)
	animation_matrix.Turn(pick(-30, 30))
	animation_matrix.Scale(0.7) // Shrink to start, end up normal sized

	pixel_x = from_x
	pixel_y = from_y
	alpha = 0
	transform = animation_matrix

	// This is instant on byond's end, but to our clients this looks like a quick drop
	animate(src, alpha = old_alpha, pixel_x = old_x, pixel_y = old_y, transform = old_transform, time = 3, easing = CUBIC_EASING)

/atom/movable/proc/get_spawner_desc()
	return name

/atom/movable/proc/get_spawner_flavour_text()
	return desc

/atom/movable/Entered(atom/movable/arrived, atom/old_loc, list/atom/old_locs)
	. = ..()

	if(LAZYLEN(arrived.important_recursive_contents))
		var/list/nested_locs = get_nested_locs(src) + src
		for(var/channel in arrived.important_recursive_contents)
			for(var/atom/movable/location as anything in nested_locs)
				LAZYORASSOCLIST(location.important_recursive_contents, channel, arrived.important_recursive_contents[channel])

/atom/movable/Exited(atom/movable/gone, direction)
	. = ..()

	if(LAZYLEN(gone.important_recursive_contents))
		var/list/nested_locs = get_nested_locs(src) + src
		for(var/channel in gone.important_recursive_contents)
			for(var/atom/movable/location as anything in nested_locs)
				LAZYREMOVEASSOC(location.important_recursive_contents, channel, gone.important_recursive_contents[channel])

///allows this movable to hear and adds itself to the important_recursive_contents list of itself and every movable loc its in
/atom/movable/proc/become_hearing_sensitive(trait_source = TRAIT_GENERIC)
	if(!HAS_TRAIT(src, TRAIT_HEARING_SENSITIVE))
		for(var/atom/movable/location as anything in get_nested_locs(src) + src)
			LAZYADDASSOCLIST(location.important_recursive_contents, RECURSIVE_CONTENTS_HEARING_SENSITIVE, src)

		var/turf/our_turf = get_turf(src)
		if(our_turf && SSspatial_grid.initialized)
			SSspatial_grid.enter_cell(src, our_turf)

		else if(our_turf && !SSspatial_grid.initialized)//SSspatial_grid isnt init'd yet, add ourselves to the queue
			SSspatial_grid.enter_pre_init_queue(src, RECURSIVE_CONTENTS_HEARING_SENSITIVE)

	ADD_TRAIT(src, TRAIT_HEARING_SENSITIVE, trait_source)

/**
 * removes the hearing sensitivity channel from the important_recursive_contents list of this and all nested locs containing us if there are no more sources of the trait left
 * since RECURSIVE_CONTENTS_HEARING_SENSITIVE is also a spatial grid content type, removes us from the spatial grid if the trait is removed
 *
 * * trait_source - trait source define or ALL, if ALL, force removes hearing sensitivity. if a trait source define, removes hearing sensitivity only if the trait is removed
 */
/atom/movable/proc/lose_hearing_sensitivity(trait_source = TRAIT_GENERIC)
	if(!HAS_TRAIT(src, TRAIT_HEARING_SENSITIVE))
		return
	REMOVE_TRAIT(src, TRAIT_HEARING_SENSITIVE, trait_source)
	if(HAS_TRAIT(src, TRAIT_HEARING_SENSITIVE))
		return

	var/turf/our_turf = get_turf(src)
	if(our_turf && SSspatial_grid.initialized)
		SSspatial_grid.exit_cell(src, our_turf)
	else if(our_turf && !SSspatial_grid.initialized)
		SSspatial_grid.remove_from_pre_init_queue(src, RECURSIVE_CONTENTS_HEARING_SENSITIVE)

	for(var/atom/movable/location as anything in get_nested_locs(src) + src)
		LAZYREMOVEASSOC(location.important_recursive_contents, RECURSIVE_CONTENTS_HEARING_SENSITIVE, src)

///allows this movable to know when it has "entered" another area no matter how many movable atoms its stuffed into, uses important_recursive_contents
/atom/movable/proc/become_area_sensitive(trait_source = TRAIT_GENERIC)
	if(!HAS_TRAIT(src, TRAIT_AREA_SENSITIVE))
		for(var/atom/movable/location as anything in get_nested_locs(src) + src)
			LAZYADDASSOCLIST(location.important_recursive_contents, RECURSIVE_CONTENTS_AREA_SENSITIVE, src)
	ADD_TRAIT(src, TRAIT_AREA_SENSITIVE, trait_source)

///removes the area sensitive channel from the important_recursive_contents list of this and all nested locs containing us if there are no more source of the trait left
/atom/movable/proc/lose_area_sensitivity(trait_source = TRAIT_GENERIC)
	if(!HAS_TRAIT(src, TRAIT_AREA_SENSITIVE))
		return
	REMOVE_TRAIT(src, TRAIT_AREA_SENSITIVE, trait_source)
	if(HAS_TRAIT(src, TRAIT_AREA_SENSITIVE))
		return

	for(var/atom/movable/location as anything in get_nested_locs(src) + src)
		LAZYREMOVEASSOC(location.important_recursive_contents, RECURSIVE_CONTENTS_AREA_SENSITIVE, src)

///propogates ourselves through our nested contents, similar to other important_recursive_contents procs
///main difference is that client contents need to possibly duplicate recursive contents for the clients mob AND its eye
/mob/proc/enable_client_mobs_in_contents()
	var/turf/our_turf = get_turf(src)

	if(our_turf && SSspatial_grid.initialized)
		SSspatial_grid.enter_cell(src, our_turf, RECURSIVE_CONTENTS_CLIENT_MOBS)
	else if(our_turf && !SSspatial_grid.initialized)
		SSspatial_grid.enter_pre_init_queue(src, RECURSIVE_CONTENTS_CLIENT_MOBS)

	for(var/atom/movable/movable_loc as anything in get_nested_locs(src) + src)
		LAZYORASSOCLIST(movable_loc.important_recursive_contents, RECURSIVE_CONTENTS_CLIENT_MOBS, src)

///Clears the clients channel of this mob
/mob/proc/clear_important_client_contents()
	var/turf/our_turf = get_turf(src)

	if(our_turf && SSspatial_grid.initialized)
		SSspatial_grid.exit_cell(src, our_turf, RECURSIVE_CONTENTS_CLIENT_MOBS)
	else if(our_turf && !SSspatial_grid.initialized)
		SSspatial_grid.remove_from_pre_init_queue(src, RECURSIVE_CONTENTS_CLIENT_MOBS)

	for(var/atom/movable/movable_loc as anything in get_nested_locs(src) + src)
		LAZYREMOVEASSOC(movable_loc.important_recursive_contents, RECURSIVE_CONTENTS_CLIENT_MOBS, src)
