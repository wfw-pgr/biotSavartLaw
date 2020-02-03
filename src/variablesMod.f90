module variablesMod
  implicit none
  integer                       :: nBpt, nLpt
  double precision              :: I0
  integer         , parameter   :: xp_=1, yp_=2, zp_=3, bx_=4, by_=5, bz_=6
  double precision, allocatable :: bfield(:,:), coilS(:,:)
  integer         , parameter   :: cLen    = 300
  integer         , parameter   ::  lun    =  50
  character(cLen)               :: prmFile = 'dat/parameter.conf'
  character(cLen)               :: outFile = 'dat/biotSavartBField.dat'
  character(cLen)               :: bptFile = 'dat/bfieldCoordinate.dat'
  character(cLen)               :: cilFile = 'dat/coilShape.dat'
  
end module variablesMod
