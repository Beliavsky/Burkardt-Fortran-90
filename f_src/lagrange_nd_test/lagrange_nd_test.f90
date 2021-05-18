program main

!*****************************************************************************80
!
!! MAIN is the main program for LAGRANGE_ND_TEST.
!
!  Discussion:
!
!    LAGRANGE_ND_TEST tests the LAGRANGE_ND library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 October 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) option

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LAGRANGE_ND_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) '  Test the LAGRANGE_ND library.'

  call test01 ( )
  call test02 ( )
  call test03 ( )
  call test04 ( )
  call test05 ( )
  call test06 ( )
  call test07 ( )
  call test08 ( )
  call test09 ( )
  call test10 ( )

  option = 0
  call test11 ( option )

  option = 1
  call test11 ( option )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LAGRANGE_ND_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests MONO_BETWEEN_ENUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 January 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) mono_between_enum
  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) v

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  MONO_BETWEEN_ENUM can enumerate the number of monomials'
  write ( *, '(a)' ) '  in D variables, of total degree between N1 and N2.'

  d = 3
  write ( *, '(a)' ) ''
  write ( *, '(a,i2)' ) '  Using spatial dimension D = ', d
  write ( *, '(a)' ) ''
  write ( *, '(a)', advance = 'no' ) '   N2:'
  do n2 = 0, 8
    write ( *, '(2x,i4)', advance = 'no' ) n2
  end do
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  N1 +------------------------------------------------------'
  do n1 = 0, 8
    write ( *, '(a,i2,a)', advance = 'no' ) '  ', n1, ' |'
    do n2 = 0, 8
      v = mono_between_enum ( d, n1, n2 )
      write ( *, '(2x,i4)', advance = 'no' ) v
    end do
    write ( *, '(a)' ) ''
  end do

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests MONO_TOTAL_ENUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 January 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) mono_total_enum
  integer ( kind = 4 ) n
  integer ( kind = 4 ) v

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  MONO_TOTAL_ENUM can enumerate the number of monomials'
  write ( *, '(a)' ) '  in D variables, of total degree N.'

  write ( *, '(a)' ) ''
  write ( *, '(a)', advance = 'no' ) '    N:'
  do n = 0, 8
    write ( *, '(i4)', advance = 'no' ) n
  end do
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   D +------------------------------------------------------'
  do d = 1, 8
    write ( *, '(2x,i2,a2)', advance = 'no' ) d, ' |'
    do n = 0, 8
      v = mono_total_enum ( d, n )
      write ( *, '(2x,i4)', advance = 'no' ) v
    end do
    write ( *, '(a)' ) ''
  end do

  return
end    
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 tests MONO_UPTO_ENUM.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 January 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) mono_upto_enum
  integer ( kind = 4 ) n
  integer ( kind = 4 ) v

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  MONO_UPTO_ENUM can enumerate the number of monomials'
  write ( *, '(a)' ) '  in D variables, of total degree 0 up to N.'

  write ( *, '(a)' ) ''
  write ( *, '(a)', advance = 'no' ) '    N:'
  do n = 0, 8
    write ( *, '(2x,i4)', advance = 'no' ) n
  end do
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '   D +------------------------------------------------------'
  do d = 1, 8
    write ( *, '(2x,i2,a2)', advance = 'no' ) d, ' |'
    do n = 0, 8
      v = mono_upto_enum ( d, n )
      write ( *, '(1x,i5)', advance = 'no' ) v
    end do
    write ( *, '(a)' ) ''
  end do

  return
end
subroutine test04 ( )

!*****************************************************************************80
!
!! TEST04 tests MONO_BETWEEN_NEXT_GRLEX.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 January 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: d = 3

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) x(d)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST04'
  write ( *, '(a)' ) '  MONO_BETWEEN_NEXT_GRLEX can list the monomials'
  write ( *, '(a)' ) '  in D variables, of total degree N between N1 and N2,'
  write ( *, '(a)' ) '  one at a time.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  We start the process with (0,0,...,0,N1).'
  write ( *, '(a)' ) '  The process ends with (N2,0,...,0,0)'

  n1 = 2
  n2 = 3

  write ( *, '(a)' ) ''
  write ( *, '(a,i3)' ) '  Let D =  ', d
  write ( *, '(a,i3)' ) '      N1 = ', n1
  write ( *, '(a,i3)' ) '      N2 = ', n2
  write ( *, '(a)' ) ''

  x = (/ 0, 0, n1 /)
  j = 1

  do

    write ( *, '(2x,i2,'':'')', advance = 'no' ) j
    do i = 1, d
      write ( *, '(2x,i1)', advance = 'no' ) x(i)
    end do
    write ( *, '(a)' ) ''

    if ( x(1) == n2 ) then
      exit
    end if

    call mono_between_next_grlex ( d, n1, n2, x )
    j = j + 1

  end do

  return
end
subroutine test05 ( )

!*****************************************************************************80
!
!! TEST05 tests LAGRANGE_ND_COMPLETE in 1D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 January 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: nd = 5

  integer ( kind = 4 ) d
  real ( kind = 8 ) error
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  character ( len = 80 ) label
  integer ( kind = 4 ) mono_upto_enum
  integer ( kind = 4 ) n
  integer ( kind = 4 ) o
  real ( kind = 8 ), allocatable :: pc(:,:)
  integer ( kind = 4 ), allocatable :: pe(:,:)
  integer ( kind = 4 ) po(nd)
  integer ( kind = 4 ) r
  real ( kind = 8 ) value(nd,nd)
  real ( kind = 8 ), dimension ( nd ) :: xd = (/ &
    0.0D+00, 1.0D+00, 2.0D+00, 3.0D+00, 4.0D+00 /)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST05'
  write ( *, '(a)' ) '  LAGRANGE_COMPLETE determines'
  write ( *, '(a)' ) '  the Lagrange interpolating polynomials L(x)'
  write ( *, '(a)' ) '  for ND points in D dimensions, assuming that'
  write ( *, '(a)' ) '  the number of points exactly coincides with'
  write ( *, '(a)' ) '  R = Pi(D,N), the number of monomials of degree N or less'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  As a special demonstration, this code runs in 1D'

  d = 1
  n = 4
  r = mono_upto_enum ( d, n )

  allocate ( pc(1:nd,1:r) )
  allocate ( pe(1:nd,1:r) )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Spatial dimension D = ', d
  write ( *, '(a,i4)' ) '  Maximum degree N = ', n
  write ( *, '(a,i4)' ) '  Number of monomials R = ', r
  write ( *, '(a,i4)' ) '  Number of data points ND = ', nd

  call r8mat_transpose_print ( d, nd, xd, '  Data points XD:' )

  call lagrange_complete ( d, n, r, nd, xd, po, pc, pe )
!
!  Print the polynomials.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Lagrange polynomials for XD data points:'
  write ( *, '(a)' ) ''
  do i = 1, nd
    o = po(i)
    write ( label, '(a,i2,a)' ) '  P(', i, ')(x) ='
    call polynomial_print ( d, o, pc(i,1:o), pe(i,1:o), label )
  end do
!
!  Evaluate the polynomials at XD.
!
  do j = 1, nd
    o = po(j)
    write ( label, '(a,i2,a)' ) '  P(', j, ')(x) ='
    call polynomial_value ( d, o, pc(j,1:o), pe(j,1:o), nd, xd, value(1:nd,j) )    
  end do

  call r8mat_is_identity ( nd, value, error )
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Frobenius norm of Lagrange matrix error = ', error

  deallocate ( pc )
  deallocate ( pe )

  return
end
subroutine test06 ( )

!*****************************************************************************80
!
!! TEST06 tests LAGRANGE_ND_COMPLETE in 2D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 January 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: d = 2
  integer ( kind = 4 ), parameter :: nd = 6

  real ( kind = 8 ) error
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  character ( len = 80 ) label
  integer ( kind = 4 ) mono_upto_enum
  integer ( kind = 4 ) n
  integer ( kind = 4 ) o
  real ( kind = 8 ), allocatable :: pc(:,:)
  integer ( kind = 4 ), allocatable :: pe(:,:)
  integer ( kind = 4 ) po(nd)
  integer ( kind = 4 ) r
  real ( kind = 8 ) value(nd,nd)
  real ( kind = 8 ), dimension ( d, nd ) :: xd = reshape ( (/ &
    0.0,  0.0, &
    1.0,  0.0, &
    2.0,  0.0, &
    0.0,  1.0, &
    1.0,  1.0, &
    0.0,  2.0 /), (/ d, nd /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST06'
  write ( *, '(a)' ) '  LAGRANGE_COMPLETE determines'
  write ( *, '(a)' ) '  the Lagrange interpolating polynomials L(x)'
  write ( *, '(a)' ) '  for ND points in D dimensions, assuming that'
  write ( *, '(a)' ) '  the number of points exactly coincides with'
  write ( *, '(a)' ) '  R = Pi(D,N), the number of monomials of degree N or less'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The data points are the grid nodes of a triangle.'

  n = 2
  r = mono_upto_enum ( d, n )

  allocate ( pc(1:nd,1:r) )
  allocate ( pe(1:nd,1:r) )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Spatial dimension D = ', d
  write ( *, '(a,i4)' ) '  Maximum degree N = ', n
  write ( *, '(a,i4)' ) '  Number of monomials R = ', r
  write ( *, '(a,i4)' ) '  Number of data points ND = ', nd

  call r8mat_transpose_print ( d, nd, xd, '  Data points XD:' )

  call lagrange_complete ( d, n, r, nd, xd, po, pc, pe )
!
!  Print the polynomials.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Lagrange polynomials for XD data points:'
  write ( *, '(a)' ) ''
  do i = 1, nd
    o = po(i)
    write ( label, '(a,i2,a)' ) '  P(', i, ')(x) ='
    call polynomial_print ( d, o, pc(i,1:o), pe(i,1:o), label )
  end do
!
!  Evaluate the polynomials at XD.
!
  do j = 1, nd
    o = po(j)
    write ( label, '(a,i2,a)' ) '  P(', j, ')(x) ='
    call polynomial_value ( d, o, pc(j,1:o), pe(j,1:o), nd, xd, value(1:nd,j) )    
  end do

  call r8mat_is_identity ( nd, value, error )
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Frobenius norm of Lagrange matrix error = ', error

  deallocate ( pc )
  deallocate ( pe )

  return
end
subroutine test07 ( )

!*****************************************************************************80
!
!! TEST07 tests LAGRANGE_ND_COMPLETE in 3D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 January 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: d = 3
  integer ( kind = 4 ), parameter :: nd = 10

  real ( kind = 8 ) error
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  character ( len = 80 ) label
  integer ( kind = 4 ) mono_upto_enum
  integer ( kind = 4 ) n
  integer ( kind = 4 ) o
  real ( kind = 8 ), allocatable :: pc(:,:)
  integer ( kind = 4 ), allocatable :: pe(:,:)
  integer ( kind = 4 ) po(nd)
  integer ( kind = 4 ) r
  real ( kind = 8 ) value(nd,nd)
  real ( kind = 8 ), dimension ( d, nd ) :: xd = reshape ( (/ &
    0.0,  0.0,  0.0, &
    1.0,  0.0,  0.0, &
    2.0,  0.0,  0.0, &
    0.0,  1.0,  0.0, &
    1.0,  1.0,  0.0, &
    0.0,  2.0,  0.0, &
    0.0,  0.0,  1.0, &
    1.0,  0.0,  1.0, &
    0.0,  1.0,  1.0, &
    0.0,  0.0,  2.0 /), (/ d, nd /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST07'
  write ( *, '(a)' ) '  LAGRANGE_COMPLETE determines'
  write ( *, '(a)' ) '  the Lagrange interpolating polynomials L(x)'
  write ( *, '(a)' ) '  for ND points in D dimensions, assuming that'
  write ( *, '(a)' ) '  the number of points exactly coincides with'
  write ( *, '(a)' ) '  R = Pi(D,N), the number of monomials of degree N or less'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  The data points are the grid nodes of a tetrahedron.'

  n = 2
  r = mono_upto_enum ( d, n )

  allocate ( pc(1:nd,1:r) )
  allocate ( pe(1:nd,1:r) )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Spatial dimension D = ', d
  write ( *, '(a,i4)' ) '  Maximum degree N = ', n
  write ( *, '(a,i4)' ) '  Number of monomials R = ', r
  write ( *, '(a,i4)' ) '  Number of data points ND = ', nd

  call r8mat_transpose_print ( d, nd, xd, '  Data points XD:' )

  call lagrange_complete ( d, n, r, nd, xd, po, pc, pe )
!
!  Print the polynomials.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Lagrange polynomials for XD data points:'
  write ( *, '(a)' ) ''
  do i = 1, nd
    o = po(i)
    write ( label, '(a,i2,a)' ) '  P(', i, ')(x) ='
    call polynomial_print ( d, o, pc(i,1:o), pe(i,1:o), label )
  end do
!
!  Evaluate the polynomials at XD.
!
  do j = 1, nd
    o = po(j)
    write ( label, '(a,i2,a)' ) '  P(', j, ')(x) ='
    call  polynomial_value ( d, o, pc(j,1:o), pe(j,1:o), nd, xd, value(1:nd,j) )    
  end do

  call r8mat_is_identity ( nd, value, error )
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Frobenius norm of Lagrange matrix error = ', error

  deallocate ( pc )
  deallocate ( pe )

  return
end
subroutine test08 ( )

!*****************************************************************************80
!
!! TEST08 tests LAGRANGE_PARTIAL in 2D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 January 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: d = 2
  integer ( kind = 4 ), parameter :: nd = 5

  real ( kind = 8 ) error
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  character ( len = 80 ) label
  integer ( kind = 4 ) mono_upto_enum
  integer ( kind = 4 ) n
  integer ( kind = 4 ) o
  real ( kind = 8 ), allocatable :: pc(:,:)
  integer ( kind = 4 ), allocatable :: pe(:,:)
  integer ( kind = 4 ) po(nd)
  integer ( kind = 4 ) r
  real ( kind = 8 ) value(nd,nd)
  real ( kind = 8 ), dimension ( d, nd ) :: xd = reshape ( (/ &
     0.0,  0.0, &
    -1.0,  0.0, &
     1.0,  0.0, &
     0.0, -1.0, &
     0.0,  1.0 /), (/ d, nd /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST08'
  write ( *, '(a)' ) '  LAGRANGE_PARTIAL determines'
  write ( *, '(a)' ) '  the Lagrange interpolating polynomials L(x)'
  write ( *, '(a)' ) '  for ND points in D dimensions, assuming that'
  write ( *, '(a)' ) '  the number of points is less than or equal to'
  write ( *, '(a)' ) '  R = Pi(D,N), the number of monomials of degree N or less'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  For this example, the data points are the same as those'
  write ( *, '(a)' ) '  used by the level 1 Clenshaw Curtis sparse grid in 2D.'

  n = 2
  r = mono_upto_enum ( d, n )

  allocate ( pc(1:nd,1:r) )
  allocate ( pe(1:nd,1:r) )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Spatial dimension D = ', d
  write ( *, '(a,i4)' ) '  Maximum degree N = ', n
  write ( *, '(a,i4)' ) '  Number of monomials R = ', r
  write ( *, '(a,i4)' ) '  Number of data points ND = ', nd

  call r8mat_transpose_print ( d, nd, xd, '  Data points XD:' )

  call lagrange_partial ( d, n, r, nd, xd, po, pc, pe )
!
!  Print the polynomials.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Lagrange polynomials for XD data points:'
  write ( *, '(a)' ) ''
  do i = 1, nd
    o = po(i)
    write ( label, '(a,i2,a)' ) '  P(', i, ')(x) ='
    call polynomial_print ( d, o, pc(i,1:o), pe(i,1:o), label )
  end do
!
!  Evaluate the polynomials at XD.
!
  do j = 1, nd
    o = po(j)
    write ( label, '(a,i2,a)' ) '  P(', j, ')(x) ='
    call polynomial_value ( d, o, pc(j,1:o), pe(j,1:o), nd, xd, value(1:nd,j) )    
  end do

  call r8mat_is_identity ( nd, value, error )
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Frobenius norm of Lagrange matrix error = ', error

  deallocate ( pc )
  deallocate ( pe )

  return
end
subroutine test09 ( )

!*****************************************************************************80
!
!! TEST09 tests LAGRANGE_PARTIAL in 3D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 January 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: d = 3
  integer ( kind = 4 ), parameter :: nd = 25

  real ( kind = 8 ) error
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  character ( len = 80 ) label
  integer ( kind = 4 ) mono_upto_enum
  integer ( kind = 4 ) n
  integer ( kind = 4 ) o
  real ( kind = 8 ), allocatable :: pc(:,:)
  integer ( kind = 4 ), allocatable :: pe(:,:)
  integer ( kind = 4 ) po(nd)
  integer ( kind = 4 ) r
  real ( kind = 8 ), parameter :: sq2h = sqrt ( 2.0D+00 ) / 2.0D+00
  real ( kind = 8 ) value(nd,nd)
  real ( kind = 8 ), dimension ( d, nd ) :: xd = reshape ( (/ &
      0.0,   0.0,   0.0, &
     -1.0,   0.0,   0.0, &
      1.0,   0.0,   0.0, &
      0.0,  -1.0,   0.0, &
      0.0,   1.0,   0.0, &
      0.0,   0.0,  -1.0, &
      0.0,   0.0,   1.0, &
     -0.707106781187 ,  0.0,   0.0, &
      0.707106781187 ,  0.0,   0.0, &
     -1.0,  -1.0,   0.0, &
      1.0,  -1.0,   0.0, &
     -1.0,   1.0,   0.0, &
      1.0,   1.0,   0.0, &
      0.0,  -0.707106781187 ,  0.0, &
      0.0,   0.707106781187 ,  0.0, &
     -1.0,   0.0,  -1.0, &
      1.0,   0.0,  -1.0, &
     -1.0,   0.0,   1.0, &
      1.0,   0.0,   1.0, &
      0.0,  -1.0,  -1.0, &
      0.0,   1.0,  -1.0, &
      0.0,  -1.0,   1.0, &
      0.0,   1.0,   1.0, &
      0.0,   0.0,  -0.707106781187 , &
      0.0,   0.0,   0.707106781187  /), (/ d, nd /) )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST09'
  write ( *, '(a)' ) '  LAGRANGE_PARTIAL determines'
  write ( *, '(a)' ) '  the Lagrange interpolating polynomials L(x)'
  write ( *, '(a)' ) '  for ND points in D dimensions, assuming that'
  write ( *, '(a)' ) '  the number of points is less than or equal to'
  write ( *, '(a)' ) '  R = Pi(D,N), the number of monomials of degree N or less'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  For this example, the data points are the same as those'
  write ( *, '(a)' ) '  used by the level 2 Clenshaw Curtis sparse grid in 3D.'

  n = 4
  r = mono_upto_enum ( d, n )

  allocate ( pc(1:nd,1:r) )
  allocate ( pe(1:nd,1:r) )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Spatial dimension D = ', d
  write ( *, '(a,i4)' ) '  Maximum degree N = ', n
  write ( *, '(a,i4)' ) '  Number of monomials R = ', r
  write ( *, '(a,i4)' ) '  Number of data points ND = ', nd

  call r8mat_transpose_print ( d, nd, xd, '  Data points XD:' )

  call lagrange_partial ( d, n, r, nd, xd, po, pc, pe )
!
!  Print the polynomials.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Lagrange polynomials for XD data points:'
  write ( *, '(a)' ) ''
  do i = 1, nd
    o = po(i)
    write ( label, '(a,i2,a)' ) '  P(', i, ')(x) ='
    call polynomial_print ( d, o, pc(i,1:o), pe(i,1:o), label )
  end do
!
!  Evaluate the polynomials at XD.
!
  do j = 1, nd
    o = po(j)
    write ( label, '(a,i2,a)' ) '  P(', j, ')(x) ='
    call polynomial_value ( d, o, pc(j,1:o), pe(j,1:o), nd, xd, value(1:nd,j) )    
  end do

  call r8mat_is_identity ( nd, value, error )
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Frobenius norm of Lagrange matrix error = ', error

  deallocate ( pc )
  deallocate ( pe )

  return
end
subroutine test10 ( )

!*****************************************************************************80
!
!! TEST10 tests LAGRANGE_PARTIAL2 in 2D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 January 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: d = 2
  integer ( kind = 4 ), parameter :: nd = 13
  integer ( kind = 4 ), parameter :: ni = 11

  real ( kind = 8 ) error
  real ( kind = 8 ) f
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  character ( len = 80 ) label
  integer ( kind = 4 ) mono_upto_enum
  integer ( kind = 4 ) n
  integer ( kind = 4 ) o
  real ( kind = 8 ), allocatable :: pc(:,:)
  real ( kind = 8 ) pd(nd)
  integer ( kind = 4 ), allocatable :: pe(:,:)
  integer ( kind = 4 ) pn
  integer ( kind = 4 ) po(nd)
  integer ( kind = 4 ) r
  real ( kind = 8 ) value(nd,nd)
  real ( kind = 8 ), dimension ( d, nd ) :: xd = reshape ( (/ &
    0.0,  0.0, &
   -1.0,  0.0, &
    1.0,  0.0, &
    0.0, -1.0, &
    0.0,  1.0, &
   -1.0,  1.0, &
    1.0,  1.0, &
   -1.0, -1.0, &
    1.0, -1.0, &
   -0.5,  0.0, &
    0.0, -0.5, &
    0.0, +0.5, &
    0.5,  0.0 /), (/ d, nd /) )
  real ( kind = 8 ) xyi(2,ni*ni)
  real ( kind = 8 ) zi(ni*ni)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST10'
  write ( *, '(a)' ) '  LAGRANGE_PARTIAL2 determines'
  write ( *, '(a)' ) '  the Lagrange interpolating polynomials L(x)\n'
  write ( *, '(a)' ) '  for ND points in D dimensions, assuming that\n'
  write ( *, '(a)' ) '  the number of points is less than or equal to\n'
  write ( *, '(a)' ) '  R = Pi(D,N), the number of monomials of degree N or less\n'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  For this example, the data points are the same as those'
  write ( *, '(a)' ) '  used by the level 2 Clenshaw Curtis sparse grid in 2D.'

  n = 4
  r = mono_upto_enum ( d, n )

  allocate ( pc(1:nd,1:r) )
  allocate ( pe(1:nd,1:r) )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Spatial dimension D = ', d
  write ( *, '(a,i4)' ) '  Maximum degree N = ', n
  write ( *, '(a,i4)' ) '  Number of monomials R = ', r
  write ( *, '(a,i4)' ) '  Number of data points ND = ', nd

  call r8mat_transpose_print ( d, nd, xd, '  Data points XD:' )

  call lagrange_partial2 ( d, n, r, nd, xd, po, pc, pe )
!
!  Print the polynomials.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Lagrange polynomials for XD data points:'
  write ( *, '(a)' ) ''
  do i = 1, nd
    o = po(i)
    write ( label, '(a,i2,a)' ) '  P(', i, ')(x) ='
    call polynomial_print ( d, o, pc(i,1:o), pe(i,1:o), label )
  end do
!
!  Evaluate the polynomials at XD.
!
  do j = 1, nd
    o = po(j)
    write ( label, '(a,i2,a)' ) '  P(', j, ')(x) ='
    call polynomial_value ( d, o, pc(j,1:o), pe(j,1:o), nd, xd, value(1:nd,j) )    
  end do

  call r8mat_is_identity ( nd, value, error )
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Frobenius norm of Lagrange matrix error = ', error
!
!  Evaluate a function at the data points.
!
  do i = 1, nd
    pd(i) = sin ( xd(1,i) ) * cos ( xd(2,i) )
  end do
!
!  Compare exact function and interpolant at a grid of points.
!
  k = 0
  do j = 1, 11
    do i = 1, 11
      k = k + 1
      xyi(1,k) = ( real ( 11 - i,     kind = 8 ) * ( - 1.0D+00 )   &
                 + real (      i - 1, kind = 8 ) * ( + 1.0D+00 ) ) &
                 / real ( 11     - 1, kind = 8 )
      xyi(2,k) = ( real ( 11 - j,     kind = 8 ) * ( - 1.0D+00 )   &
                 + real (      j - 1, kind = 8 ) * ( + 1.0D+00 ) ) &
                 / real ( 11     - 1, kind = 8 )
    end do
  end do

  pn = nd
  call interpolant_value ( d, r, pn, po, pc, pe, pd, ni, xyi, zi )

  error = 0.0D+00
  do k = 1, ni
    f = sin ( xyi(1,k) ) * cos ( xyi(2,k) )
    error = max ( error, abs ( zi(k) - f ) )
  end do
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) &
    '  Maximum absolute interpolant error on 11x11 grid = ', error

  deallocate ( pc )
  deallocate ( pe )

  return
end
subroutine test11 ( option )

!*****************************************************************************80
!
!! LAGRANGE_ND_TEST11 tests LAGRANGE_PARTIAL3 in 2D.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    28 October 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) OPTION, determines the initial basis:
!    0, use monomials, 1, x, y, x^2, xy, y^2, x^3, ...
!    1, use Legendre products, 1, y, x, (3y^2-1)/2, xy, (3^x^2-1), (5y^3-3)/2,...
!
  implicit none

  interface
    subroutine lagrange_partial3 ( d, n, nd, xd, option, po, pc, pe, n2 )
      integer ( kind = 4 ) d
      integer ( kind = 4 ) n
      integer ( kind = 4 ) nd
      real ( kind = 8 ) xd(d,nd)
      integer ( kind = 4 ) option
      integer ( kind = 4 ) po(nd)
      real ( kind = 8 ), allocatable :: pc(:,:)
      integer ( kind = 4 ), allocatable :: pe(:,:)
      integer ( kind = 4 ) n2
    end subroutine
  end interface

  integer ( kind = 4 ), parameter :: d = 2
  integer ( kind = 4 ), parameter :: nd = 65
  integer ( kind = 4 ), parameter :: ni = 11 * 11

  real ( kind = 8 ) error
  real ( kind = 8 ) f
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  character ( len = 80 ) label
  integer ( kind = 4 ) mono_upto_enum
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n2
  integer ( kind = 4 ) o
  integer ( kind = 4 ) option
  real ( kind = 8 ), allocatable :: pc(:,:)
  real ( kind = 8 ) pd(nd)
  integer ( kind = 4 ), allocatable :: pe(:,:)
  integer ( kind = 4 ) pn
  integer ( kind = 4 ) po(nd)
  integer ( kind = 4 ) r
  real ( kind = 8 ) value(nd,nd)
  real ( kind = 8 ), dimension ( d, nd ) :: xd = reshape ( (/ &
      0.0000000000000000,      0.0000000000000000, &
     -1.0000000000000000,      0.0000000000000000, &
      1.0000000000000000,      0.0000000000000000, &
      0.0000000000000000,     -1.0000000000000000, &
      0.0000000000000000,      1.0000000000000000, &
     -0.7071067811865475,      0.0000000000000000, &
      0.7071067811865476,      0.0000000000000000, &
     -1.0000000000000000,     -1.0000000000000000, &
      1.0000000000000000,     -1.0000000000000000, &
     -1.0000000000000000,      1.0000000000000000, &
      1.0000000000000000,      1.0000000000000000, &
      0.0000000000000000,     -0.7071067811865475, &
      0.0000000000000000,      0.7071067811865476, &
     -0.9238795325112867,      0.0000000000000000, &
     -0.3826834323650897,      0.0000000000000000, &
      0.3826834323650898,      0.0000000000000000, &
      0.9238795325112867,      0.0000000000000000, &
     -0.7071067811865475,     -1.0000000000000000, &
      0.7071067811865476,     -1.0000000000000000, &
     -0.7071067811865475,      1.0000000000000000, &
      0.7071067811865476,      1.0000000000000000, &
     -1.0000000000000000,     -0.7071067811865475, &
      1.0000000000000000,     -0.7071067811865475, &
     -1.0000000000000000,      0.7071067811865476, &
      1.0000000000000000,      0.7071067811865476, &
      0.0000000000000000,     -0.9238795325112867, &
      0.0000000000000000,     -0.3826834323650897, &
      0.0000000000000000,      0.3826834323650898, &
      0.0000000000000000,      0.9238795325112867, &
     -0.9807852804032304,      0.0000000000000000, &
     -0.8314696123025453,      0.0000000000000000, &
     -0.5555702330196020,      0.0000000000000000, &
     -0.1950903220161282,      0.0000000000000000, &
      0.1950903220161283,      0.0000000000000000, &
      0.5555702330196023,      0.0000000000000000, &
      0.8314696123025452,      0.0000000000000000, &
      0.9807852804032304,      0.0000000000000000, &
     -0.9238795325112867,     -1.0000000000000000, &
     -0.3826834323650897,     -1.0000000000000000, &
      0.3826834323650898,     -1.0000000000000000, &
      0.9238795325112867,     -1.0000000000000000, &
     -0.9238795325112867,      1.0000000000000000, &
     -0.3826834323650897,      1.0000000000000000, &
      0.3826834323650898,      1.0000000000000000, &
      0.9238795325112867,      1.0000000000000000, &
     -0.7071067811865475,     -0.7071067811865475, &
      0.7071067811865476,     -0.7071067811865475, &
     -0.7071067811865475,      0.7071067811865476, &
      0.7071067811865476,      0.7071067811865476, &
     -1.0000000000000000,     -0.9238795325112867, &
      1.0000000000000000,     -0.9238795325112867, &
     -1.0000000000000000,     -0.3826834323650897, &
      1.0000000000000000,     -0.3826834323650897, &
     -1.0000000000000000,      0.3826834323650898, &
      1.0000000000000000,      0.3826834323650898, &
     -1.0000000000000000,      0.9238795325112867, &
      1.0000000000000000,      0.9238795325112867, &
      0.0000000000000000,     -0.9807852804032304, &
      0.0000000000000000,     -0.8314696123025453, &
      0.0000000000000000,     -0.5555702330196020, &
      0.0000000000000000,     -0.1950903220161282, &
      0.0000000000000000,      0.1950903220161283, &
      0.0000000000000000,      0.5555702330196023, &
      0.0000000000000000,      0.8314696123025452, &
      0.0000000000000000,      0.9807852804032304 /), (/ d, nd /) )
  real ( kind = 8 ) xyi(d,ni)
  real ( kind = 8 ) zi(ni)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LAGRANGE_ND_TEST11'
  write ( *, '(a)' ) '  LAGRANGE_PARTIAL3 determines'
  write ( *, '(a)' ) '  the Lagrange interpolating polynomials L(x)'
  write ( *, '(a)' ) '  for ND points in D dimensions, assuming that'
  write ( *, '(a)' ) '  the number of points is less than or equal to'
  write ( *, '(a)' ) '  R = Pi(D,N), the number of monomials of degree N or less'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  If LAGRANGE_PARTIAL3 determines that the problem is not'
  write ( *, '(a)' ) '  well-posed for the given value of N, it increases N'
  write ( *, '(a)' ) '  until a suitable value is found.'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  For this example, the data points are the same as those'
  write ( *, '(a)' ) '  used by the level 2 Clenshaw Curtis sparse grid in 2D.'

  n = 10
  r = mono_upto_enum ( d, n )

  write ( *, '(a)' ) ''
  write ( *, '(a,i4)' ) '  Spatial dimension D = ', d
  write ( *, '(a,i4)' ) '  Maximum degree N = ', n
  write ( *, '(a,i4)' ) '  Number of monomials R = ', r
  write ( *, '(a,i4)' ) '  Number of data points ND = ', nd
  write ( *, '(a,i4)' ) '  Monomial/Legendre option OPTION = ', option

  call r8mat_transpose_print ( d, nd, xd, '  Data points XD:' )
!
!  Note that PC and PE are allocated within LAGRANGE_PARTIAL3.
!
  call lagrange_partial3 ( d, n, nd, xd, option, po, pc, pe, n2 )

  if ( n < n2 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a,i4)' ) '  LAGRANGE_PARTIAL3 increased N to ', n2
    r = mono_upto_enum ( d, n2 )
  end if
!
!  Print the polynomials.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  (First 2) Lagrange polynomials for XD data points:'
  write ( *, '(a)' ) ''

! do i = 1, nd
  do i = 1, 2
    o = po(i)
    write ( label, '(a,i2,a)' ) '  P(', i, ')(x) ='
    call polynomial_print ( d, o, pc(i,1:o), pe(i,1:o), label )
  end do
!
!  Evaluate the polynomials at XD.
!
  do j = 1, nd
    o = po(j)
    write ( label, '(a,i2,a)' ) '  P(', j, ')(x) ='
    call polynomial_value ( d, o, pc(j,1:o), pe(j,1:o), nd, xd, value(1:nd,j) )    
  end do

  call r8mat_is_identity ( nd, value, error )
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  Frobenius norm of Lagrange matrix error = ', error
!
!  Evaluate a function at the data points.
!
  do i = 1, nd
    pd(i) = exp ( xd(1,i) * xd(2,i) )
  end do
!
!  Compare exact function and interpolant at a grid of points.
!
  k = 0
  do j = 1, 11
    do i = 1, 11
      k = k + 1
      xyi(1,k) = ( real ( 11 - i,     kind = 8 ) * ( - 1.0D+00 )   &
                 + real (      i - 1, kind = 8 ) * ( + 1.0D+00 ) ) &
                 / real ( 11     - 1, kind = 8 )
      xyi(2,k) = ( real ( 11 - j,     kind = 8 ) * ( - 1.0D+00 )   &
                 + real (      j - 1, kind = 8 ) * ( + 1.0D+00 ) ) &
                 / real ( 11     - 1, kind = 8 )
    end do
  end do

  pn = nd
  call interpolant_value ( d, r, pn, po, pc, pe, pd, ni, xyi, zi )

  error = 0.0D+00
  do k = 1, ni
    f = exp ( xyi(1,k) * xyi(2,k) )
    error = max ( error, abs ( zi(k) - f ) )
  end do
  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) &
    '  Maximum absolute interpolant error on 11x11 grid = ', error
!
!  Free memory.
!
  deallocate ( pc )
  deallocate ( pe )

  return
end
