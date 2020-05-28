************************************************************************
* /MBTOOLS/IF_MANIFEST
* MBT Manifest
*
* (c) MBT 2020 https://marcbernardtools.com/
************************************************************************
INTERFACE /mbtools/if_manifest
  PUBLIC .


  TYPES:
    BEGIN OF ty_component,
      component  TYPE dlvunit,
      release    TYPE saprelease,
      extrelease TYPE sappatchlv,
    END OF ty_component .
  TYPES:
    ty_components TYPE STANDARD TABLE OF ty_component WITH KEY component .
  TYPES:
    BEGIN OF ty_parameter,
      parameter_name  TYPE string,
      paramater_value TYPE string,
    END OF ty_parameter .
  TYPES:
    ty_parameters TYPE STANDARD TABLE OF ty_parameter WITH KEY parameter_name .
  TYPES:
    BEGIN OF ty_certificate,
      certificate_name TYPE string,
      certificate_url  TYPE string,
    END OF ty_certificate .
  TYPES:
    ty_certificates TYPE STANDARD TABLE OF ty_certificate WITH KEY certificate_name .
  TYPES:
    BEGIN OF ty_descriptor,
      id           TYPE i,
      name         TYPE string,
      version      TYPE string,
      title        TYPE string,
      description  TYPE string,
      namespace    TYPE namespace,
      package      TYPE devclass,
      class        TYPE seoclsname,
      components   TYPE ty_components,
      parameters   TYPE ty_parameters,
      certificates TYPE ty_certificates,
    END OF ty_descriptor .

  CONSTANTS co_file_name TYPE string VALUE '.mbt-manifest.xml' ##NO_TEXT.
  CONSTANTS co_interface_version TYPE i VALUE 1 ##NO_TEXT.
  DATA descriptor TYPE ty_descriptor READ-ONLY .
ENDINTERFACE.
