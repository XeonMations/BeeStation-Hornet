#define STATE_CUSTOMIZING_MENU "customizing_menu"
#define STATE_VIEWING_MENU "viewing_menu"

/obj/machinery/kitchen_menu
	name = "kitchen menu"
	desc = "It's a holographic sign with the menu for today."
	icon = 'icons/obj/machines/kitchen.dmi'
	icon_state = "menu"
	density = FALSE
	pass_flags_self = PASSMACHINE | PASSTABLE| LETPASSTHROW
	use_power = IDLE_POWER_USE
	idle_power_usage = 5
	layer = BELOW_OBJ_LAYER
	req_one_access = list(ACCESS_KITCHEN)
	circuit = /obj/item/circuitboard/machine/kitchen_menu
	resistance_flags = FIRE_PROOF

	// The current state of the UI
	var/state = STATE_VIEWING_MENU

/obj/machinery/kitchen_menu/ui_interact(mob/user, datum/tgui/ui)
	ui = SStgui.try_update_ui(user, src, ui)
	if(!ui)
		ui = new(user, src, "KitchenMenu")
		state = STATE_VIEWING_MENU
		ui.open()

/obj/machinery/kitchen_menu/ui_data(mob/user)
	var/list/data = list()

	data["authorized"] = allowed(user) //Do we have the chef's authorization to add or remove menu items to the menu?
	data["page"] = state
	return data

/obj/machinery/kitchen_menu/ui_act(action, list/params)
	var/static/list/approved_states = list(STATE_VIEWING_MENU, STATE_CUSTOMIZING_MENU)

	. = ..()
	if (.)
		return

	switch(action)
		if("add_menu_item")
			. = TRUE
		if("setState")
			if(!allowed(usr))
				return
			if(!(params["state"] in approved_states))
				return
			state = params["state"]
			playsound(src, "terminal_type", 50, FALSE)
			. = TRUE
