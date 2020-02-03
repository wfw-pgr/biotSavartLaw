program main
  use variablesMod
  use ioUtilityMod, only : load__parameters, load__bfieldposition
  use ioUtilityMod, only : load__coilShape , save__results
  use biotSavarMod, only : calc__biotSavartBField
  implicit none

  call load__parameters
  call load__bfieldposition
  call load__coilShape
  call calc__biotSavartBField( BField, coilS, I0, nBpt, nLpt )
  call save__results
  
end program main
