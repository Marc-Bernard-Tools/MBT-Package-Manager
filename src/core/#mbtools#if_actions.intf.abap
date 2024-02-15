INTERFACE /mbtools/if_actions
  PUBLIC .


************************************************************************
* Marc Bernard Tools - Actions
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
************************************************************************
  CONSTANTS quit TYPE string VALUE 'quit' ##NO_TEXT.
  CONSTANTS go_home TYPE string VALUE 'go_home' ##NO_TEXT.
  CONSTANTS go_back TYPE string VALUE 'go_back' ##NO_TEXT.
  " Pages
  CONSTANTS go_admin TYPE string VALUE 'go_admin' ##NO_TEXT.
  CONSTANTS go_license TYPE string VALUE 'go_license' ##NO_TEXT.
  CONSTANTS go_faq TYPE string VALUE 'go_faq' ##NO_TEXT.
  CONSTANTS go_about TYPE string VALUE 'go_about' ##NO_TEXT.
  " Internet
  CONSTANTS url TYPE string VALUE 'url?url=' ##NO_TEXT.
  CONSTANTS mbt_portfolio TYPE string VALUE 'mbt_portfolio' ##NO_TEXT.
  CONSTANTS mbt_docs TYPE string VALUE 'mbt_docs' ##NO_TEXT.
  CONSTANTS mbt_support TYPE string VALUE 'mbt_support' ##NO_TEXT.
  CONSTANTS mbt_license TYPE string VALUE 'mbt_license' ##NO_TEXT.
  CONSTANTS mbt_website TYPE string VALUE 'mbt_website' ##NO_TEXT.
  CONSTANTS mbt_account TYPE string VALUE 'mbt_account' ##NO_TEXT.
  " Objects
  CONSTANTS show_object TYPE string VALUE 'show_object' ##NO_TEXT.
  CONSTANTS run_program TYPE string VALUE 'run_program' ##NO_TEXT.
  CONSTANTS run_transaction TYPE string VALUE 'run_transaction' ##NO_TEXT.
  " Exceptions
  CONSTANTS goto_message TYPE string VALUE 'goto_message' ##NO_TEXT.
  CONSTANTS goto_source TYPE string VALUE 'goto_source' ##NO_TEXT.
  CONSTANTS show_callstack TYPE string VALUE 'show_callstack' ##NO_TEXT.
  CONSTANTS tools_check TYPE string VALUE 'tools_check' ##NO_TEXT.
  CONSTANTS tools_update TYPE string VALUE 'tools_update' ##NO_TEXT.
  CONSTANTS tool_check TYPE string VALUE 'tool_check' ##NO_TEXT.
  CONSTANTS tool_update TYPE string VALUE 'tool_update' ##NO_TEXT.
  CONSTANTS tool_docs TYPE string VALUE 'tool_docs' ##NO_TEXT.
  CONSTANTS tool_info TYPE string VALUE 'tool_info' ##NO_TEXT.
  CONSTANTS tool_changelog TYPE string VALUE 'tool_changelog' ##NO_TEXT.
  CONSTANTS tool_launch TYPE string VALUE 'tool_launch' ##NO_TEXT.
  CONSTANTS tool_activate TYPE string VALUE 'tool_activate' ##NO_TEXT.
  CONSTANTS tool_deactivate TYPE string VALUE 'tool_deactivate' ##NO_TEXT.
  CONSTANTS tool_register TYPE string VALUE 'tool_register' ##NO_TEXT.
  CONSTANTS tool_unregister TYPE string VALUE 'tool_unregister' ##NO_TEXT.
  CONSTANTS tool_install TYPE string VALUE 'tool_install' ##NO_TEXT.
  CONSTANTS tool_uninstall TYPE string VALUE 'tool_uninstall' ##NO_TEXT.
  CONSTANTS tool_license TYPE string VALUE 'tool_license' ##NO_TEXT.
  CONSTANTS tool_sync TYPE string VALUE 'tool_sync' ##NO_TEXT.
  CONSTANTS license_add TYPE string VALUE 'license_add' ##NO_TEXT.
  CONSTANTS license_remove TYPE string VALUE 'license_remove' ##NO_TEXT.
  CONSTANTS repo_add_online TYPE string VALUE 'repo_add_online' ##NO_TEXT.
  CONSTANTS repo_add_offline TYPE string VALUE 'repo_add_offline' ##NO_TEXT.
  CONSTANTS repo_remove TYPE string VALUE 'repo_remove' ##NO_TEXT.
ENDINTERFACE.
