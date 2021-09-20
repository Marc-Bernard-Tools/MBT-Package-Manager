REPORT /mbtools/where_used.
************************************************************************
* Marc Bernard Tools
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************

" Just a place to collect references to otherwise unused types and variables
DATA gs_used TYPE /mbtools/object_with_icon_text.
WRITE: / gs_used-obj_name.

WRITE: / /mbtools/cl_string_map=>c_version,
         /mbtools/cl_string_map=>c_origin,
         /mbtools/cl_string_map=>c_license.

WRITE: / /mbtools/cl_registry=>c_version.
