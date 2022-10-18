REPORT /mbtools/where_used.

************************************************************************
* Marc Bernard Tools - Where Used
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: GPL-3.0-only
************************************************************************

" Just a place to collect references to otherwise unused types and variables
DATA gs_used TYPE /mbtools/object_with_icon_text.
WRITE: / gs_used-obj_name.

WRITE: / /mbtools/cl_registry=>c_version.
