program main

!*****************************************************************************80
!
!! MAIN is the main program for SFTPACK_TEST.
!
!  Discussion:
!
!    SFTPACK_TEST tests the SFTPACK library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 June 2015
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SFTPACK_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the SFTPACK library.'

  call c4mat_sft_test ( )
  call c4vec_sft_test ( )

  call c8mat_sft_test ( )
  call c8vec_sft_test ( )

  call r4vec_sft_test ( )

  call r8vec_sct_test ( )
  call r8vec_sft_test ( 35 )
  call r8vec_sft_test ( 36 )
  call r8vec_sht_test ( )
  call r8vec_sqct_test ( )
  call r8vec_sqst_test ( )
  call r8vec_sst_test ( )
  call r8vec_swt_test ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'SFTPACK_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine c4mat_sft_test ( )

!*****************************************************************************80
!
!! C4MAT_SFT_TEST tests C4MAT_SFTB and C4MAT_SFTF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 June 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n1 = 10
  integer ( kind = 4 ), parameter :: n2 = 4

  integer ( kind = 4 ) seed
  complex ( kind = 4 ) x(n1,n2)
  complex ( kind = 4 ) x2(n1,n2)
  complex ( kind = 4 ) y(n1,n2)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C4MAT_SFT_TEST'
  write ( *, '(a)' ) '  C4MAT_SFTF computes the forward slow Fourier transform.'
  write ( *, '(a)' ) '  C4MAT_SFTB computes the backward slow Fourier transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8,a,i8)' ) &
    '  The data has dimension N1 = ', n1, ' by N2 = ', n2

  seed = 123456789

  call c4mat_uniform_01 ( n1, n2, seed, x )

  call c4mat_print_some ( n1, n2, x, 1, 1, 10, 10, '  The data X:' )
!
!  Compute the slow Fourier transform of the data.
!
  call c4mat_sftf ( n1, n2, x, y )

  call c4mat_print_some ( n1, n2, y, 1, 1, 10, 10, '  The Fourier coefficients Y:' )

  call c4mat_sftb ( n1, n2, y, x2 )

  call c4mat_print_some ( n1, n2, x2, 1, 1, 10, 10, '  The recovered data:' )

  return
end
subroutine c4vec_sft_test ( )

!*****************************************************************************80
!
!! C4VEC_SFT tests C4VEC_SFTB and C4VEC_SFTF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 June 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 36

  integer ( kind = 4 ) seed
  complex ( kind = 4 ) x(n)
  complex ( kind = 4 ) x2(n)
  complex ( kind = 4 ) y(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C4VEC_SFT_TEST'
  write ( *, '(a)' ) '  C4VEC_SFTF computes the forward slow Fourier transform.'
  write ( *, '(a)' ) '  C4VEC_SFTB computes the backward slow Fourier transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  The number of data values, N = ', n

  seed = 123456789

  call c4vec_uniform_01 ( n, seed, x )

  call c4vec_print_part ( n, x, 10, '  The data X:' )
!
!  Compute the slow Fourier transform of the data.
!
  call c4vec_sftf ( n, x, y )

  call c4vec_print_part ( n, y, 10, '  The Fourier coefficients Y:' )

  call c4vec_sftb ( n, y, x2 )

  call c4vec_print_part ( n, x2, 10, '  The recovered data:' )

  return
end
subroutine c8mat_sft_test ( )

!*****************************************************************************80
!
!! C8MAT_SFT_TEST tests C8MAT_SFTB and C8MAT_SFTF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 June 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n1 = 10
  integer ( kind = 4 ), parameter :: n2 = 4

  integer ( kind = 4 ) seed
  complex ( kind = 8 ) x(n1,n2)
  complex ( kind = 8 ) x2(n1,n2)
  complex ( kind = 8 ) y(n1,n2)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8MAT_SFT_TEST'
  write ( *, '(a)' ) '  C8MAT_SFTF computes the forward slow Fourier transform.'
  write ( *, '(a)' ) '  C8MAT_SFTB computes the backward slow Fourier transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8,a,i8)' ) &
    '  The data has dimension N1 = ', n1, ' by N2 = ', n2

  seed = 123456789

  call c8mat_uniform_01 ( n1, n2, seed, x )

  call c8mat_print_some ( n1, n2, x, 1, 1, 10, 10, '  The data X:' )
!
!  Compute the slow Fourier transform of the data.
!
  call c8mat_sftf ( n1, n2, x, y )

  call c8mat_print_some ( n1, n2, y, 1, 1, 10, 10, '  The Fourier coefficients Y:' )

  call c8mat_sftb ( n1, n2, y, x2 )

  call c8mat_print_some ( n1, n2, x2, 1, 1, 10, 10, '  The recovered data:' )

  return
end

subroutine c8vec_sft_test ( )

!*****************************************************************************80
!
!! C8VEC_SFT_TEST tests C8VEC_SFTB and C8VEC_SFTF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 June 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 36

  integer ( kind = 4 ) seed
  complex ( kind = 8 ) x(n)
  complex ( kind = 8 ) x2(n)
  complex ( kind = 8 ) y(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'C8VEC_SFT_TEST'
  write ( *, '(a)' ) '  C8VEC_SFTF computes the forward slow Fourier transform.'
  write ( *, '(a)' ) '  C8VEC_SFTB computes the backward slow Fourier transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  The number of data values, N = ', n

  seed = 123456789

  call c8vec_uniform_01 ( n, seed, x )

  call c8vec_print_part ( n, x, 10, '  The data X:' )
!
!  Compute the slow Fourier transform of the data.
!
  call c8vec_sftf ( n, x, y )

  call c8vec_print_part ( n, y, 10, '  The Fourier coefficients Y:' )

  call c8vec_sftb ( n, y, x2 )

  call c8vec_print_part ( n, x2, 10, '  The recovered data:' )

  return
end
subroutine r4vec_sft_test ( )

!*****************************************************************************80
!
!! R4VEC_SFT_TEST tests R4VEC_SFTB and R4VEC_SFTF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 June 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 36

  real ( kind = 4 ) a(n/2)
  real ( kind = 4 ), parameter :: ahi = 5.0E+00
  real ( kind = 4 ), parameter :: alo = 0.0E+00
  real ( kind = 4 ) azero
  real ( kind = 4 ) b(n/2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed
  real ( kind = 4 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R4VEC_SFT_TEST'
  write ( *, '(a)' ) '  R4VEC_SFTF computes the forward slow Fourier transform.'
  write ( *, '(a)' ) '  R4VEC_SFTB computes the backward slow Fourier transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  The number of data values, N = ', n

  seed = 123456789

  call r4vec_uniform_ab ( n, alo, ahi, seed, x )

  call r4vec_print_part ( n, x, 10, '  The original data:' )
!
!  Compute the slow Fourier transform of the data.
!
  call r4vec_sftf ( n, x, azero, a, b )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  A (cosine) coefficients:'
  write ( *, '(a)' ) ' '

  write ( *, '(2x,i3,g14.6)' ) 0, azero

  do i = 1, n/2
    write ( *, '(2x,i3,g14.6)' ) i, a(i)
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  B (sine) coefficients:'
  write ( *, '(a)' ) ' '

  do i = 1, n/2
    write ( *, '(2x,i3,g14.6)' ) i, b(i)
  end do
!
!  Now try to retrieve the data from the coefficients.
!
  call r4vec_sftb ( n, azero, a, b, x )

  call r4vec_print_part ( n, x, 10, '  The retrieved data:' )

  return
end
subroutine r8vec_sct_test ( )

!*****************************************************************************80
!
!! R8VEC_SCT_TEST tests R8VEC_SCT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 February 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 256

  real ( kind = 8 ), parameter :: ahi = 5.0D+00
  real ( kind = 8 ), parameter :: alo = 0.0D+00
  real ( kind = 8 ) c(n)
  real ( kind = 8 ) d(n)
  real ( kind = 8 ) e(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8VEC_SCT_TEST'
  write ( *, '(a)' ) '  R8VEC_SCT does a forward or backward slow cosine transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  The number of data items is N = ', n
!
!  Set the data values.
!
  seed = 123456789

  call r8vec_uniform_ab ( n, alo, ahi, seed, c )

  call r8vec_print_part ( n, c, 10, '  The original data:' )
!
!  Compute the coefficients.
!
  call r8vec_sct ( n, c, d )

  call r8vec_print_part ( n, d, 10, '  The cosine coefficients:' )
!
!  Now compute inverse transform of coefficients.  Should get back the
!  original data.
!
  call r8vec_sct ( n, d, e )

  e(1:n) = e(1:n) / real ( 2 * n, kind = 8 )

  call r8vec_print_part ( n, e, 10, '  The retrieved data:' )

  return
end
subroutine r8vec_sft_test ( n )

!*****************************************************************************80
!
!! R8VEC_SFT_TEST tests R8VEC_SFTB and R8VEC_SFTF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 February 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n/2)
  real ( kind = 8 ), parameter :: ahi = 5.0D+00
  real ( kind = 8 ), parameter :: alo = 0.0D+00
  real ( kind = 8 ) azero
  real ( kind = 8 ) b(n/2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8VEC_SFT_TEST'
  write ( *, '(a)' ) '  R8VEC_SFTF computes the forward slow Fourier transform.'
  write ( *, '(a)' ) '  R8VEC_SFTB computes the backward slow Fourier transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  The number of data values, N = ', n

  seed = 123456789

  call r8vec_uniform_ab ( n, alo, ahi, seed, x )

  call r8vec_print_part ( n, x, 10, '  The original data:' )
!
!  Compute the slow Fourier transform of the data.
!
  call r8vec_sftf ( n, x, azero, a, b )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  A (cosine) coefficients:'
  write ( *, '(a)' ) ' '

  write ( *, '(2x,i3,g14.6)' ) 0, azero

  do i = 1, n/2
    write ( *, '(2x,i3,g14.6)' ) i, a(i)
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  B (sine) coefficients:'
  write ( *, '(a)' ) ' '

  do i = 1, n/2
    write ( *, '(2x,i3,g14.6)' ) i, b(i)
  end do
!
!  Now try to retrieve the data from the coefficients.
!
  call r8vec_sftb ( n, azero, a, b, x )

  call r8vec_print_part ( n, x, 10, '  The retrieved data:' )

  return
end
subroutine r8vec_sht_test ( )

!*****************************************************************************80
!
!! R8VEC_SHT_TEST tests R8VEC_SHT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 February 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 17

  real ( kind = 8 ), parameter :: ahi = 5.0D+00
  real ( kind = 8 ), parameter :: alo = 0.0D+00
  real ( kind = 8 ) c(n)
  real ( kind = 8 ) d(n)
  real ( kind = 8 ) e(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8VEC_SHT_TEST'
  write ( *, '(a)' ) '  R8VEC_SHT does a forward or backward slow Hartley transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  The number of data items is N = ', n
!
!  Set the data values.
!
  seed = 123456789

  call r8vec_uniform_ab ( n, alo, ahi, seed, c )

  call r8vec_print_part ( n, c, 10, '  The original data:' )
!
!  Compute the coefficients.
!
  call r8vec_sht ( n, c, d )

  call r8vec_print_part ( n, d, 10, '  The Hartley coefficients:' )
!
!  Now compute inverse transform of coefficients.  Should get back the
!  original data.
!
  call r8vec_sht ( n, d, e )

  call r8vec_print_part ( n, e, 10, '  The retrieved data:' )

  return
end
subroutine r8vec_sqct_test ( )

!*****************************************************************************80
!
!! R8VEC_SQCT_TEST tests R8VEC_SQCTB and R8VEC_SQCTF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 February 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 256

  real ( kind = 8 ), parameter :: ahi = 5.0D+00
  real ( kind = 8 ), parameter :: alo = 0.0D+00
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8VEC_SQCT_TEST'
  write ( *, '(a)' ) '  R8VEC_SQCTF does a forward slow quarter wave cosine transform;'
  write ( *, '(a)' ) '  R8VEC_SQCTB does a backward slow quarter wave cosine transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  The number of data items is N = ', n
!
!  Set the data values.
!
  seed = 123456789

  call r8vec_uniform_ab ( n, alo, ahi, seed, x )

  call r8vec_print_part ( n, x, 10, '  The original data:' )
!
!  Compute the coefficients.
!
  call r8vec_sqctf ( n, x, y )

  call r8vec_print_part ( n, y, 10, '  The cosine coefficients:' )
!
!  Now compute inverse transform of coefficients.  Should get back the
!  original data.
!
  call r8vec_sqctb ( n, y, x )

  call r8vec_print_part ( n, x, 10, '  The retrieved data:' )

  return
end
subroutine r8vec_sqst_test ( )

!*****************************************************************************80
!
!! R8VEC_SQST_TEST tests R8VEC_SQSTB and R8VEC_SQSTF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 February 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 256

  real ( kind = 8 ), parameter :: ahi = 5.0D+00
  real ( kind = 8 ), parameter :: alo = 0.0D+00
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n)
  real ( kind = 8 ) y(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8VEC_SQST_TEST'
  write ( *, '(a)' ) '  R8VEC_SQSTF does a forward slow quarter wave sine transform;'
  write ( *, '(a)' ) '  R8VEC_SQSTB does a backward slow quarter wave sine transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  The number of data items is N = ', n
!
!  Set the data values.
!
  seed = 123456789

  call r8vec_uniform_ab ( n, alo, ahi, seed, x )

  call r8vec_print_part ( n, x, 10, '  The original data:' )
!
!  Compute the coefficients.
!
  call r8vec_sqstf ( n, x, y )

  call r8vec_print_part ( n, y, 10, '  The sine coefficients:' )
!
!  Now compute inverse transform of coefficients.  Should get back the
!  original data.
!
  call r8vec_sqstb ( n, y, x )

  call r8vec_print_part ( n, x, 10, '  The retrieved data:' )

  return
end
subroutine r8vec_sst_test ( )

!*****************************************************************************80
!
!! R8VEC_SST_TEST tests R8VEC_SST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 February 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 256

  real ( kind = 8 ), parameter :: ahi = 5.0D+00
  real ( kind = 8 ), parameter :: alo = 0.0D+00
  real ( kind = 8 ) c(n)
  real ( kind = 8 ) d(n)
  real ( kind = 8 ) e(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'R8VEC_SST_TEST'
  write ( *, '(a)' ) '  R8VEC_SST does a forward or backward slow sine transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  The number of data items is N = ', n
!
!  Set the data values.
!
  seed = 123456789

  call r8vec_uniform_ab ( n, alo, ahi, seed, c )

  call r8vec_print_part ( n, c, 10, '  The original data:' )
!
!  Compute the coefficients.
!
  call r8vec_sst ( n, c, d )

  call r8vec_print_part ( n, d, 10, '  The sine coefficients:' )
!
!  Now compute inverse transform of coefficients.  Should get back the
!  original data.
!
  call r8vec_sst ( n, d, e )

  e(1:n) = e(1:n) / real ( 2 * ( n + 1 ), kind = 8 )

  call r8vec_print_part ( n, e, 10, '  The retrieved data:' )

  return
end
subroutine r8vec_swt_test ( )

!*****************************************************************************80
!
!! R8VEC_SWT_TEST tests R8VEC_SWTB and R8VEC_SWTF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 August 2010
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 36
! integer ( kind = 4 ), parameter :: np1h = ( ( n + 1 ) / 2 )
  integer ( kind = 4 ), parameter :: np1h = 18

  real ( kind = 8 ) ahi
  real ( kind = 8 ) alo
  real ( kind = 8 ) d(np1h)
  integer ( kind = 4 ) i
  real ( kind = 8 ) s(np1h)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) x(n)

  alo = 0.0D+00
  ahi = 5.0D+00

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'R8VEC_SWT_TEST'
  write ( *, '(a)' ) '  R8VEC_SWTF computes the forward slow wavelet transform.'
  write ( *, '(a)' ) '  R8VEC_SWTB computes the backward slow wavelet transform.'
  write ( *, '(a)' ) ''
  write ( *, '(a,i6)' ) '  The number of data values, N = ', n

  seed = 123456789

  call r8vec_uniform_ab ( n, alo, ahi, seed, x )

  call r8vec_print_part ( n, x, 10, '  The original data:' )
!
!  Compute the slow wavelet transform of the data.
!
  call r8vec_swtf ( n, x, s, d )

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     I          S(I)            D(I)'
  write ( *, '(a)' ) ''
  do i = 1, np1h
    write ( *, '(2x,i4,2x,f14.6,2x,f14.6)' ) i, s(i), d(i)
  end do
!
!  Now try to retrieve the data from the coefficients.
!
  call r8vec_swtb ( n, s, d, x )

  call r8vec_print_part ( n, x, 10, '  The retrieved data:' )

  return
end

