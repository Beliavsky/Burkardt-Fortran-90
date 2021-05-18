subroutine beta ( y, delta, gamma )

!*****************************************************************************80
!
!! beta() is a subroutine.
!
!  real function zeta ( x ) is commented out.
!
  real delta
  real gamma
  character ( len = 50 ) s
  real y

  gamma = 7.0
  y = 3.14159265
!
!  This line might confuse the extractor!
!
  s = 'function gamma ( 17 )'

  return
end
