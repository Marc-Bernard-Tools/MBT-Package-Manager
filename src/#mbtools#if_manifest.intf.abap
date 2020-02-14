INTERFACE /mbtools/if_manifest
  PUBLIC .

  TYPES:
    BEGIN OF ty_component,
      component  TYPE dlvunit,
      release    TYPE saprelease,
      extrelease TYPE sappatchlv,
    END OF ty_component,
    ty_components TYPE STANDARD TABLE OF ty_component WITH KEY component,

    BEGIN OF ty_parameter,
      parameter_name  TYPE string,
      paramater_value TYPE string,
    END OF ty_parameter,
    ty_parameters TYPE STANDARD TABLE OF ty_parameter WITH KEY parameter_name,

    BEGIN OF ty_certificate,
      certificate_name TYPE string,
      certificate_url  TYPE string,
    END OF ty_certificate,
    ty_certificates TYPE STANDARD TABLE OF ty_certificate WITH KEY certificate_name,

    BEGIN OF ty_descriptor,
      id           TYPE i,
      name         TYPE string,
      version      TYPE string,
      description  TYPE string,
      mbt_url      TYPE string,
      namespace    TYPE namespace,
      package      TYPE devclass,
      components   TYPE ty_components,
      parameters   TYPE ty_parameters,
      certificates TYPE ty_certificates,
    END OF ty_descriptor.

  CONSTANTS:
    co_file_name         TYPE string VALUE '.mbt-manifest.xml',
    co_interface_version TYPE i VALUE 1.

  DATA:
    descriptor TYPE ty_descriptor READ-ONLY.

ENDINTERFACE.
