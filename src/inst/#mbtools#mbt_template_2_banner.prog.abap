*&---------------------------------------------------------------------*
*&  Include           /MBTOOLS/MBT_TEMPLATE_2_BANNER
*&---------------------------------------------------------------------*

CONSTANTS:
  c_banner_id   TYPE string VALUE '/MBTOOLS/MBT_INSTALLER',
  c_title       TYPE string VALUE 'MBT Installer',
  c_url_docs    TYPE string VALUE 'https://marcbernardtools.com/docs/marc-bernard-tools/installation/',
  c_url_license TYPE string VALUE 'https://marcbernardtools.com/company/terms-software/',
  c_url_repo    TYPE string VALUE 'https://marcbernardtools.com/'.

CONSTANTS:
  c_tabname TYPE tabname  VALUE 'ZMBTINST',
  c_lock    TYPE viewname VALUE 'EZMBTINST'.

FORM banner.
  INSERT 'iVBORw0KGgoAAAANSUhEUgAAAMgAAAAdCAIAAABgybRVAAAALHRFWHRDcmVhdGlvbiBUaW1lAEZyaSAy' INTO TABLE gt_banner.
  INSERT 'NyBOb3YgMjAyMCAyMDoyNjoxOCAtMDUwMAhjuEsAAAAHdElNRQfkCxwBHSIH0G3NAAAACXBIWXMAAAsS' INTO TABLE gt_banner.
  INSERT 'AAALEgHS3X78AAAABGdBTUEAALGPC/xhBQAAAAZ0Uk5TAO4A7gDuHYxM+gAABeBJREFUeNrtmzFM41YY' INTO TABLE gt_banner.
  INSERT 'x30IxMJwMFCdmJwERYroUKVeUjFhEXFZDql4QrK6BC8kZIhOsuhEFXHKEI4shgVZuslLuqQnX8MU3S1u' INTO TABLE gt_banner.
  INSERT 'VIYoUtoknhiOISwsLLR+thM/J3Gwk5dLAv4NyHnv8fdnv4/v+94HvPD5fLVaDUPK8fHx4uIiWs2ZmZmH' INTO TABLE gt_banner.
  INSERT 'hweEgoqFJEmiNdKlzSxyr8LUPUOuqXhVNBpFKHh+fo7cSJc2M+M2wOVp4jqWy0hwHctlJMyO24Bnj1zg' INTO TABLE gt_banner.
  INSERT 'TvNVcOWPxBgSR6uFUt0RFo4Vip4cJuNbPu1T7eN7+vXBl1D0D/4MGkv/dnD+xfadbi4/XPx1bTW78mNk' INTO TABLE gt_banner.
  INSERT '8/u15WVHxt/+fXlZv7WxcDH488aA71TmEkyuol4GkmJmBMfIWo7nJXBB+JWtR62FUt0JvRwrdPLv57gP' INTO TABLE gt_banner.
  INSERT 'GvBtxT//FzcvUsbOtiKBn1YPbPvWdaPRsJprNLJFwbO+/3Z3zYn5t81m08ayoQ6pFUlSdwYLDKPy3Oiu' INTO TABLE gt_banner.
  INSERT 'sUInvMmr+uGL8ychdMY0itl3lzfjfiUuKOiKWNHDtluBbPd7JfDGyIlg8P1eGhrzxQ+jB68ddoQ8nvWV' INTO TABLE gt_banner.
  INSERT 'FdPIdbGoBbOG8Km8YTtqvfR6vVA0ur2t6wFsackL99KWXo7j3T5rOh0rFFjVr2pqXaVcnJexdmr8uLd6' INTO TABLE gt_banner.
  INSERT 'cG4eWw0oMct+qQVYiex2+s7mq3e/CqpvFa/Ku2v2PGsR/8FUOsmX9T81x1oMbgxaVD2GUg6LGAPKFVku' INTO TABLE gt_banner.
  INSERT 'iKdprQALbCdjYRLvuidYI+ar1c5xfyzD4KCAO61WJH1Myp0mqp1LoPsmmDSvZ2WMoFNJvRhXJkSvaand' INTO TABLE gt_banner.
  INSERT 'B4EFYUXzY6ofCmI+X+1plwXOToW1f8pDb0xvlr9TQlhjeJ1vQC3HsrkqHajoVbGKJFE8q+wNB710mQsH' INTO TABLE gt_banner.
  INSERT 'Wam3CB3JYDhWr/I8bwxKsKK+xEJJ4llww5SQrFIUTwvtpfYoJMIUbzatpVgStScAj1n1lSJ5pmMlZFcf' INTO TABLE gt_banner.
  INSERT 'JqPdcFP+kC/q155Xzo6GY8HsAsYoG8R8Te3oWEhYepUz+vinxFLUQIIUb2GaxAYTrSfAMJ4K8vZ1TYzF' INTO TABLE gt_banner.
  INSERT 'sYrZvaLV3HpkYwocqw1IIBFfLZ9m9a3iqURE2RiZS7e2hCDoQNeB0u8FX71+mqaNyEfQdKBzicwxrClb' INTO TABLE gt_banner.
  INSERT 'hZXhupKCWX4wt4UFFUWBi3kx8TTI6uYSqVjPpoRhm2bXY0xGxGrhoY6ctRvGSztvkCQZ9rfiCp8vZEjj' INTO TABLE gt_banner.
  INSERT '5dN9ul84k8nIXEUvdIjt7upFFnMtL6CFtpDyfdANHQEJtswvJFpeBe7Ro36ihVLGaW91sn6l0xAupqjd' INTO TABLE gt_banner.
  INSERT 'QGyHjbeNh7cJ/bJSkzHc14o9PLUUhkgkuIJs/x71atuvImb3xJkkPYDRkGAS+JCaF/XnSZV6/QiAGOb8' INTO TABLE gt_banner.
  INSERT 'IDSWiLW+f7Rpync3ny6yWr/BWbthgiEjNNYqzCUJrvKV+gyqkfsj1yr6FdGdgbx+xZUdxqwOQSgv9g5W' INTO TABLE gt_banner.
  INSERT 'AzOmiLVsZm33F8qjTxWvRnXy/LaQmVKKsJqUWIazE7eMwNcDI/jYBxasG+cLi2A1BJNVYz0tcEZsMnKh' INTO TABLE gt_banner.
  INSERT 'UIfGamlK20upqgyDCKFutXkIoh2VpJwoM6a+lnE8cIIhaJwo9WBV4DgvgypqjcWxrr+WzVHp61VeaDWx' INTO TABLE gt_banner.
  INSERT 'pqLdYANZ1kKS12vKYYGu7GUkTZ4KY+opTQUH7VZQurGS5ghBMNtuijKDNTMMQR0tWAFB0IPIYfZy9KOM' INTO TABLE gt_banner.
  INSERT 'xbEaQjZrNTdl7QYrlOKl/5kNqpnIWIrg9TgGdY5oAfSTQI3O6uW1EmSC7LCWwYKaaHCJhT9BfawhmKxT' INTO TABLE gt_banner.
  INSERT '4ZS1GwaHSHFQXFBypkBb1mP9ijWCtv4+S/oLCmiKrYlxLM86tX/09klEq0dQ9q473ZAZsSSkaKLnhiuO' INTO TABLE gt_banner.
  INSERT 'p06aVIBMU8xkuJRz1wLVX6nTl1VFZH9x9mIUr+7s7GwUssj/S2dnZ2cUdo4SrXLDcXSNAfSKKu6pcLpA' INTO TABLE gt_banner.
  INSERT '7gAjUFSZmFTo8rRwHctlJLiO5TISZhcWFu7u7tCK3t/fz8/PI7cV7T/Fz83NIbfQpc3/q1uUaEN2fD4A' INTO TABLE gt_banner.
  INSERT 'AAAASUVORK5CYIIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA' INTO TABLE gt_banner.
  INSERT 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA                    ' INTO TABLE gt_banner.
ENDFORM.
