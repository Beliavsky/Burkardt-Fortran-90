program main

!*****************************************************************************80
!
!! VAN_DER_CORPUT_TEST tests the VAN_DER_CORPUT library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 August 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VAN_DER_CORPUT_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the VAN_DER_CORPUT library.'

  call vdc_test ( )
  call vdc_inverse_test ( )
  call vdc_sequence_test ( )
  call vdc_base_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VAN_DER_CORPUT_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine vdc_base_test ( )

!*****************************************************************************80
!
!! VDC_BASE_TEST tests VDC_BASE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 August 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) r2
  real ( kind = 8 ) r3
  real ( kind = 8 ) r5
  real ( kind = 8 ) vdc_base

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VDC_BASE_TEST'
  write ( *, '(a)' ) '  VDC_BASE returns the I-th element of a van der Corput'
  write ( *, '(a)' ) '  sequence in base B.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    I          VDC_BASE(I,2)   VDC_BASE(I,3)   VDC_BASE(I,5)'
  write ( *, '(a)' ) ''

  do i = -10, 10
    r2 = vdc_base ( i, 2 )
    r3 = vdc_base ( i, 3 )
    r5 = vdc_base ( i, 5 )
    write ( *, '(2x,i3,13x,g14.8,2x,g14.8,2x,g14.8)' ) i, r2, r3, r5
  end do

  return
end
subroutine vdc_inverse_test ( )

!*****************************************************************************80
!
!! VDC_INVERSE_TEST tests VDC_INVERSE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 August 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i2
  real ( kind = 8 ) r
  real ( kind = 8 ) vdc
  integer ( kind = 4 ) vdc_inverse

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VDC_INVERSE_TEST'
  write ( *, '(a)' ) '  VDC_INVERSE inverts an element of a van der Corput sequence.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    I        R=VDC(I)  VDC_INVERSE(R)'
  write ( *, '(a)' ) ''

  do i = -10, 10
    r = vdc ( i )
    i2 = vdc_inverse ( r )
    write ( *, '(2x,i3,2x,g14.8,2x,i3)' ) i, r, i2
  end do

  return
end
subroutine vdc_sequence_test ( )

!*****************************************************************************80
!
!! VDC_SEQUENCE_TEST tests VDC_SEQUENCE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 August 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2
  integer ( kind = 4 ) n
  real ( kind = 8 ), allocatable :: r(:)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VDC_SEQUENCE_TEST'
  write ( *, '(a)' ) '  VDC_SEQUENCE returns elements I1 through I2 of '
  write ( *, '(a)' ) '  a van der Corput sequence.'

  i1 = 7
  i2 = 7
  n = abs ( i2 - i1 ) + 1
  allocate ( r(1:n) )
  call vdc_sequence ( i1, i2, r )
  write ( *, '(a)' ) ''
  call r8vec_transpose_print ( n, r, '  R=VDC_SEQUENCE(  7,  7):' )
  deallocate ( r )

  i1 = 0
  i2 = 8
  n = abs ( i2 - i1 ) + 1
  allocate ( r(1:n) )
  call vdc_sequence ( i1, i2, r )
  write ( *, '(a)' ) ''
  call r8vec_transpose_print ( n, r, '  R=VDC_SEQUENCE(  0,  8):' )
  deallocate ( r )

  i1 = 8
  i2 = 0
  n = abs ( i2 - i1 ) + 1
  allocate ( r(1:n) )
  call vdc_sequence ( i1, i2, r )
  write ( *, '(a)' ) ''
  call r8vec_transpose_print ( n, r, '  R=VDC_SEQUENCE(  8,  0):' )
  deallocate ( r )

  i1 = -3
  i2 = +5
  n = abs ( i2 - i1 ) + 1
  allocate ( r(1:n) )
  call vdc_sequence ( i1, i2, r )
  write ( *, '(a)' ) ''
  call r8vec_transpose_print ( n, r, '  R=VDC_SEQUENCE( -3,  5):' )
  deallocate ( r )

  i1 = 100
  i2 = 105
  n = abs ( i2 - i1 ) + 1
  allocate ( r(1:n) )
  call vdc_sequence ( i1, i2, r )
  write ( *, '(a)' ) ''
  call r8vec_transpose_print ( n, r, '  R=VDC_SEQUENCE(100,105):' )
  deallocate ( r )

  return
end
subroutine vdc_test ( )

!*****************************************************************************80
!
!! VDC_TEST tests VDC.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 August 2016
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) r
  real ( kind = 8 ) vdc

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'VDC_TEST'
  write ( *, '(a)' ) '  VDC returns the I-th element of a van der Corput sequence.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '    I          VDC(I)'
  write ( *, '(a)' ) ''

  do i = -10, 10
    r = vdc ( i )
    write ( *, '(2x,i3,2x,f14.8)' ) i, r
  end do

  return
end
