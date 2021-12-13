'OHRRPGCE GAME - In-App Purchases/Donations menu
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#ifndef PURCHASE_BI
#define PURCHASE_BI

DECLARE SUB purchases_menu ()
DECLARE FUNCTION get_enabled_store_name () as string
DECLARE FUNCTION sanity_check_store_name(storename as string) as string
DECLARE FUNCTION product_enabled_for_current_store(byval prod as NodePtr) as bool
DECLARE SUB do_purchase_action (byval prod as NodePtr, byval new_purch as bool=YES)
DECLARE FUNCTION get_persist_reld() as NodePtr
DECLARE SUB write_persist_reld()
DECLARE SUB close_persist_reld()
DECLARE FUNCTION supports_in_app_purchases () as bool
DECLARE SUB refresh_purchases()

#endif
