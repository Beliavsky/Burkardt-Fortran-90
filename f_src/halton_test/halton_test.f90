program main

!*****************************************************************************80
!
!! MAIN is the main program for HALTON_TEST.
!
!  Discussion:
!
!    HALTON_TEST tests the HALTON library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HALTON_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the HALTON library'

  call halton_test ( )
  call halton_sequence_test ( )
  call halton_inverse_test ( )
  call halton_base_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HALTON_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine halton_test ( )

!*****************************************************************************80
!
!! HALTON_TEST tests HALTON.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) m
  real ( kind = 8 ) r(3)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HALTON_TEST'
  write ( *, '(a)' ) '  HALTON returns the I-th element of an M-dimensional'
  write ( *, '(a)' ) '  Halton sequence.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    I          HALTON(I)'

  do m = 1, 3
    write ( *, '(a)' ) ''
    write ( *, '(a,i2)' ) '  Use M = ', m
    write ( *, '(a)' ) ''
    do i = 0, 10
      call  halton ( i, m, r )
      write ( *, '(2x,i3,3(2x,f14.8))' ) i, r(1:m)
    end do
  end do

  return
end
subroutine halton_base_test ( )

!*****************************************************************************80
!
!! HALTON_BASE_TEST tests HALTON_BASE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) b(3)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) m
  real ( kind = 8 ) r(3)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HALTON_BASE_TEST'
  write ( *, '(a)' ) '  HALTON_BASE returns the I-th element of an M-dimensional'
  write ( *, '(a)' ) '  Halton sequence, using user-specified bases.'

  m = 3
  b = (/ 2, 3, 5 /)
  write ( *, '(a)' ) ''
  write ( *, '(a,i3)' ) '  M = ', m
  write ( *, '(a,3(2x,i14))' ) '   B:', b(1:m)
  do i = 0, 10
    call  halton_base ( i, m, b, r )
    write ( *, '(2x,i3,3(2x,f14.8))' ) i, r(1:m)
  end do

  m = 3
  b = (/ 3, 10, 2 /)
  write ( *, '(a)' ) ''
  write ( *, '(a,i3)' ) '  M = ', m
  write ( *, '(a,3(2x,i14))' ) '   B:', b(1:m)
  do i = 0, 10
    call  halton_base ( i, m, b, r )
    write ( *, '(2x,i3,3(2x,f14.8))' ) i, r(1:m)
  end do

  return
end
subroutine halton_inverse_test ( )

!*****************************************************************************80
!
!! HALTON_INVERSE_TEST tests HALTON_INVERSE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) m
  real ( kind = 8 ) r(3)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HALTON_INVERSE_TEST'
  write ( *, '(a)' ) '  HALTON_INVERSE inverts an element of a Halton sequence.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    I        R=HALTON(I,3)  HALTON_INVERSE(R,3)'
  write ( *, '(a)' ) ''

  m = 3

  do i = 0, 10
    call halton ( i, m, r )
    call halton_inverse ( r, m, i2 )
    write ( *, '(2x,i3,3(2x,f14.8),2x,i3)' ) i, r(1:m), i2
  end do

  return
end
subroutine halton_sequence_test ( )

!*****************************************************************************80
!
!! HALTON_SEQUENCE_TEST tests HALTON_SEQUENCE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 August 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) m
  real ( kind = 8 ) r(3,11)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HALTON_SEQUENCE_TEST'
  write ( *, '(a)' ) '  HALTON_SEQUENCE returns the elements I1 through I2'
  write ( *, '(a)' ) '  of an M-dimensional Halton sequence.'

  do m = 1, 3
    write ( *, '(a)' ) ''
    write ( *, '(a,i1,a)' ) '  HALTON_SEQUENCE(0,10,', m, ',R):'
    call halton_sequence ( 0, 10, m, r )
    call r8mat_print ( m, 11, r, '  R:' )
  end do

  m = 3
  write ( *, '(a)' ) ''
  write ( *, '(a,i1,a)' ) '  HALTON_SEQUENCE(10,0,', m, ',R):'
  call halton_sequence ( 10, 0, m, r )
  call r8mat_print ( m, 11, r, '  R:' )

  return
end

