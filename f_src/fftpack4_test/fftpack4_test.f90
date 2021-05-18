program main

!*****************************************************************************80
!
!! MAIN is the main program for FFTPACK4_TEST.
!
!  Discussion:
!
!    FFTPACK4_TEST calls the FFTPACK4 test routines.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 January 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'FFTPACK4_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the FFTPACK4 library.'

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

  call test11 ( )
  call test12 ( )
  call test13 ( )
  call test14 ( )
  call test15 ( )
  call test16 ( )
  call test17 ( )
  call test18 ( )
  call test19 ( )
  call test20 ( )

  call test21 ( )
  call test22 ( )
  call test23 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'FFTPACK4_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests CFFTB, CFFTF, CFFTI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4096

  real ( kind = 4 ), parameter :: ahi = 5.0E+00
  integer ( kind = 4 ) seed
  real ( kind = 4 ) wsave(4*n+15)
  complex ( kind = 4 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  For complex fast Fourier transform,'
  write ( *, '(a)' ) '  CFFTI initializes the transform,'
  write ( *, '(a)' ) '  CFFTF does a forward transform;'
  write ( *, '(a)' ) '  CFFTB does a backward transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i8)' ) '  The number of data items is N = ', n
!
!  Set the data values.
!
  seed = 1973

  call c4vec_uniform_01 ( n, seed, x )

  x(1:n) = ahi * x(1:n)

  call c4vec_print_some ( n, x, 10, '  The original data:' )
!
!  Initialize the WSAVE array.
!
  call cffti ( n, wsave )
!
!  Compute the FFT coefficients.
!
  call cfftf ( n, x, wsave )

  call c4vec_print_some ( n, x, 10, '  The FFT coefficients:' )
!
!  Now compute inverse FFT of coefficients.  Should get back the
!  original data.

  call cfftb ( n, x, wsave )

  x(1:n) = x(1:n) / real ( n, kind = 4 )

  call c4vec_print_some ( n, x, 10, '  The retrieved data:' )

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 tests CFFTB_2D, CFFTF_2D.
!
!  Discussion:
!
!    Plot the image and transform of an 8 by 8 unit source
!    in a 64 by 64 array.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 64
  integer ( kind = 4 ), parameter :: lda = n

  real ( kind = 4 ) a(n,n)
  real ( kind = 4 ) dat
  real ( kind = 4 ) ermax
  real ( kind = 4 ) err
  integer ( kind = 4 ) i
  complex ( kind = 4 ) image(lda,n)
  complex ( kind = 4 ) image2(lda,n)
  integer ( kind = 4 ) j
  real ( kind = 4 ) wsave(4*n+15)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  For complex fast Fourier transform of 2D data:'
  write ( *, '(a)' ) '  CFFTF_2D computes the forward transform;'
  write ( *, '(a)' ) '  CFFTB_2D computes the backward transform.'
  write ( *, '(a)' ) ' '
!
!  Initialize the WSAVE array.
!
  call cffti ( n, wsave )
!
!  Set up the data.
!
!    IMAGE is the original data.
!
!    IMAGE2 is IMAGE scaled by (-1)^(I+J), to place the Fourier coefficients
!    in the correct place for viewing.
!
  do i = 1, n
    do j = 1, n

      if ( (n/2-4) <= i .and. i <= (n/2+4) .and. &
           (n/2-4) <= j .and. j <= (n/2+4) ) then
        a(i,j) = 1.0E+00
      else
        a(i,j) = 0.0E+00
      end if

      image(i,j) = cmplx ( a(i,j), 0.0E+00 )
      image2(i,j) = image(i,j) * (-1)**(i-1+j-1)

    end do
  end do
!
!  Compute the forward Fourier transform of IMAGE and IMAGE2.
!
  call cfftf_2d ( lda, n, image, wsave )

  call cfftf_2d ( lda, n, image2, wsave )
!
!  Compute the magnitude of the components of transform.
!  The actual transform are unscaled and need to be divided by N*N
!  to be correct.
!
  a(1:n,1:n) = abs ( image(1:n,1:n) )
!
!  Compute the inverse Fourier transform of IMAGE.
!
  call cfftb_2d ( lda, n, image, wsave )
!
!  The transform need to be divided by N*N to be correct.
!
  image(1:n,1:n) = image(1:n,1:n) / real ( n**2, kind = 4 )
!
!  See if the result agrees with the original data.
!
  ermax = 0.0E+00

  do i = 1, n
    do j = 1, n

      if ( (n/2-4) <= i .and. i <= (n/2+4) .and. &
           (n/2-4) <= j .and. j <= (n/2+4) ) then
        dat = 1.0E+00
      else
        dat = 0.0E+00
      end if

      err = abs ( dat - abs ( image(i,j) ) )

      ermax = max ( ermax, err )

    end do
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Maximum error in CFFT2D calculation:'
  write ( *, '(a)' ) ' '
  write ( *, '(g14.6)' ) ermax

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 tests COSQB, COSQF, COSQI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4096

  real ( kind = 4 ), parameter :: ahi = 5.0E+00
  real ( kind = 4 ), parameter :: alo = 0.0E+00
  integer ( kind = 4 ) seed
  real ( kind = 4 ) wsave(3*n+15)
  real ( kind = 4 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  For real fast cosine quarter wave transform,'
  write ( *, '(a)' ) '  COSQI initializes the transform.'
  write ( *, '(a)' ) '  COSQF does a forward transform;'
  write ( *, '(a)' ) '  COSQB does a backward transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  The number of data items is N = ', n
!
!  Set the data values.
!
  seed = 1973

  call r4vec_uniform ( n, alo, ahi, seed, x )

  call r4vec_print_some ( n, x, 10, '  The original data:' )
!
!  Initialize the WSAVE array.
!
  call cosqi ( n, wsave )
!
!  Compute the coefficients.
!
  call cosqf ( n, x, wsave )

  call r4vec_print_some ( n, x, 10, '  The cosine coefficients:' )
!
!  Now compute inverse transform of coefficients.  Should get back the
!  original data.
!
  call cosqb ( n, x, wsave )

  x(1:n) = x(1:n) / real ( 4 * n, kind = 4 )

  call r4vec_print_some ( n, x, 10, '  The retrieved data:' )

  return
end
subroutine test04 ( )

!*****************************************************************************80
!
!! TEST04 tests R4_SQSTB, R4_SQSTF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4096

  real ( kind = 4 ), parameter :: ahi = 5.0E+00
  real ( kind = 4 ), parameter :: alo = 0.0E+00
  integer ( kind = 4 ) seed
  real ( kind = 4 ) x(n)
  real ( kind = 4 ) y(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST04'
  write ( *, '(a)' ) '  For real slow quarter wave cosine transform,'
  write ( *, '(a)' ) '  R4_SQCTF does a forward transform;'
  write ( *, '(a)' ) '  R4_SQCTB does a backward transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  The number of data items is N = ', n
!
!  Set the data values.
!
  seed = 1973

  call r4vec_uniform ( n, alo, ahi, seed, x )

  call r4vec_print_some ( n, x, 10, '  The original data:' )
!
!  Compute the coefficients.
!
  call r4_sqctf ( n, x, y )

  call r4vec_print_some ( n, y, 10, '  The cosine coefficients:' )
!
!  Now compute inverse transform of coefficients.  Should get back the
!  original data.

  call r4_sqctb ( n, y, x )

  call r4vec_print_some ( n, x, 10, '  The retrieved data:' )

  return
end
subroutine test05 ( )

!*****************************************************************************80
!
!! TEST05 tests EZFFTB, EZFFTF, EZFFTI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4096

  real ( kind = 4 ) a(n/2)
  real ( kind = 4 ), parameter :: ahi = 5.0E+00
  real ( kind = 4 ), parameter :: alo = 0.0E+00
  real ( kind = 4 ) azero
  real ( kind = 4 ) b(n/2)
  integer ( kind = 4 ) seed
  real ( kind = 4 ) wsave(3*n+15)
  real ( kind = 4 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST05'
  write ( *, '(a)' ) '  For real fast Fourier transform,'
  write ( *, '(a)' ) '  EZFFTI initializes the transform.'
  write ( *, '(a)' ) '  EZFFTF does a forward transform;'
  write ( *, '(a)' ) '  EZFFTB does a backward transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  The number of data items is N = ', n
!
!  Set the data values.
!
  seed = 1973

  call r4vec_uniform ( n, alo, ahi, seed, x )

  call r4vec_print_some ( n, x, 10, '  The original data:' )
!
!  Initialize the WSAVE array.
!
  call ezffti ( n, wsave )
!
!  Compute FFT
!
  call ezfftf ( n, x, azero, a, b, wsave )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  The A0 coefficient:'
  write ( *, '(a)' ) ' '
  write ( *, '(g14.6)' ) azero

  call r4vec_print_some ( n/2, a, 10, '  The A coefficients:' )

  call r4vec_print_some ( n/2, b, 10, '  The B coefficients:' )
!
!  Now compute inverse FFT of coefficients.  Should get back the
!  original data.  First destroy original data so we're sure
!  that the routine had to recreate them!
!
  x(1:n) = 0.0E+00

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Retrieve data from FFT coeficients.'

  call ezfftb ( n, x, azero, a, b, wsave )

  call r4vec_print_some ( n, x, 10, '  The retrieved data:' )

  return
end
subroutine test06 ( )

!*****************************************************************************80
!
!! TEST06 tests EZFFTB, EZFFTF, EZFFTI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 3087
  integer ( kind = 4 ), parameter :: nhalf = 1543

  real ( kind = 4 ) a(nhalf)
  real ( kind = 4 ), parameter :: ahi = 5.0E+00
  real ( kind = 4 ), parameter :: alo = 0.0E+00
  real ( kind = 4 ) azero
  real ( kind = 4 ) b(nhalf)
  integer ( kind = 4 ) seed
  real ( kind = 4 ) wsave(3*n+15)
  real ( kind = 4 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST06'
  write ( *, '(a)' ) '  For real fast Fourier transform,'
  write ( *, '(a)' ) '  EZFFTI initializes the transform.'
  write ( *, '(a)' ) '  EZFFTF does a forward transform;'
  write ( *, '(a)' ) '  EZFFTB does a backward transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  The number of data items is N = ', n
  write ( *, '(a)' ) '  which is not a multiple of two!'
!
!  Set the data values.
!
  seed = 1973

  call r4vec_uniform ( n, alo, ahi, seed, x )

  call r4vec_print_some ( n, x, 10, '  The original data:' )
!
!  Initialize the WSAVE array.
!
  call ezffti ( n, wsave )
!
!  Compute FFT
!
  call ezfftf ( n, x, azero, a, b, wsave )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  The A0 coefficient:'
  write ( *, '(a)' ) ' '
  write ( *, '(g14.6)' ) azero

  call r4vec_print_some ( nhalf, a, 10, '  The A coefficients:' )

  call r4vec_print_some ( nhalf, b, 10, '  The B coefficients:' )
!
!  Now compute inverse FFT of coefficients.  Should get back the
!  original data.  First destroy original data so we're sure
!  that the routine had to recreate them!
!
  x(1:n) = 0.0E+00

  call ezfftb ( n, x, azero, a, b, wsave )

  call r4vec_print_some ( n, x, 10, '  The retrieved data:' )

  return
end
subroutine test07 ( )

!*****************************************************************************80
!
!! TEST07 tests EZFFTF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 36

  real ( kind = 4 ) a(n/2)
  real ( kind = 4 ) azero
  real ( kind = 4 ) b(n/2)
  real ( kind = 4 ) error
  real ( kind = 4 ) wsave(3*n+15)
  real ( kind = 4 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST07'
  write ( *, '(a)' ) '  EZFFTF can take the Fourier transform of a real vector'
  write ( *, '(a)' ) '  of data.  In this case, the vector is'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    (1,1,1,...,1)'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  and the transform should be'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    (N,0,0,...,0),'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  where N is the number of entries, ', n

  x(1:n) = 1.0E+00
!
!  Initialize the WSAVE array.
!
  call ezffti ( n, wsave )
!
!  Compute the Fourier transform of the data.
!
  call ezfftf ( n, x, azero, a, b, wsave )
!
!  Compute the maximum error.
!
  error = max ( &
             abs ( 1.0E+00 - azero ), &
    maxval ( abs ( a(1:n/2) ) ), &
    maxval ( abs ( b(1:n/2) ) ) )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  The maximum error in computation is ', error

  return
end
subroutine test08 ( )

!*****************************************************************************80
!
!! TEST08 tests EZFFTF, R4_SFTF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 36

  real ( kind = 4 ) a_f(n/2)
  real ( kind = 4 ) a_s(n/2)
  real ( kind = 4 ), parameter :: ahi = 5.0E+00
  real ( kind = 4 ), parameter :: alo = 0.0E+00
  real ( kind = 4 ) azero_f
  real ( kind = 4 ) azero_s
  real ( kind = 4 ) b_f(n/2)
  real ( kind = 4 ) b_s(n/2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed
  real ( kind = 4 ) wsave(3*n+15)
  real ( kind = 4 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST08'
  write ( *, '(a)' ) '  For real data,'
  write ( *, '(a)' ) '  EZFFTF takes the fast Fourier transform forward.'
  write ( *, '(a)' ) '  R4_SFTF computes the "slow" Fourier transform forward.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  The number of data values, N = ', n

  seed = 1973

  call r4vec_uniform ( n, alo, ahi, seed, x )
!
!  Initialize the WSAVE array.
!
  call ezffti ( n, wsave )
!
!  Compute the fast Fourier transform of the data.
!
  call ezfftf ( n, x, azero_f, a_f, b_f, wsave )
!
!  Compute the slow Fourier transform of the data.
!
  call r4_sftf ( n, x, azero_s, a_s, b_s )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Fast    Slow'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  A coefficients:'
  write ( *, '(a)' ) ' '

  write ( *, '(i3,2g14.6)' ) 0, azero_f, azero_s

  do i = 1, n/2
    write ( *, '(i3,2g14.6)' ) i, a_f(i), a_s(i)
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  B coefficients:'
  write ( *, '(a)' ) ' '

  do i = 1, n/2
    write ( *, '(i3,2g14.6)' ) i, b_f(i), b_s(i)
  end do

  return
end
subroutine test09 ( )

!*****************************************************************************80
!
!! TEST09 tests EZFFTB.
!
!  Discussion:
!
!    The input (1,0,0,...,0) should produce the output (1,0,0,...,0).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 36

  real ( kind = 4 ) a(n/2)
  real ( kind = 4 ) azero
  real ( kind = 4 ) b(n/2)
  real ( kind = 4 ) error
  real ( kind = 4 ) wsave(3*n+15)
  real ( kind = 4 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST09'
  write ( *, '(a)' ) '  EZFFTB can be used to recover a real data vector'
  write ( *, '(a)' ) '  from a Fourier coefficient vector.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  In this test, the Fourier coefficient vector is:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    (1,0,0,...,0)'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  and the recovered data vector should be'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    (1,1,1,...,1).'

  azero = 1.0E+00
  a(1:n/2) = 0.0E+00
  b(1:n/2) = 0.0E+00
!
!  Initialize the WSAVE array.
!
  call ezffti ( n, wsave )
!
!  Compute the inverse Fourier transform.
!
  call ezfftb ( n, x, azero, a, b, wsave )
!
!  Compute the maximum error.
!
  error = maxval ( abs ( x(1:n) - 1.0E+00 ) )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  The maximum error in the computation was ', error

  return
end
subroutine test10 ( )

!*****************************************************************************80
!
!! TEST10 tests RFFTF.
!
!  Discussion:
!
!    The input vector is (1,1,1,...,1) which should produce
!    the output (N,0,0,...,0).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 36

  real ( kind = 4 ) error
  real ( kind = 4 ) wsave(2*n+15)
  real ( kind = 4 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST10'
  write ( *, '(a)' ) '  RFFTF can compute the Fourier transform of a real'
  write ( *, '(a)' ) '  vector of data.  In this case, the vector is'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    (1,1,1,...,1)'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  and the transform should be'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    (N,0,0,...,0), '
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  where N is the number of entries, N = ', n

  x(1:n) = 1.0E+00
!
!  Initialize the WSAVE array.
!
  call rffti ( n, wsave )
!
!  Compute the Fourier transform.
!
  call rfftf ( n, x, wsave )
!
!  Test results.
!
  error = max ( &
    abs ( real ( n, kind = 4 ) - x(1) ), &
    maxval ( abs ( x(2:n) ) ) )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  The maximum error in computation is ', error

  return
end
subroutine test11 ( )

!*****************************************************************************80
!
!! TEST11 tests RFFTB.
!
!  Discussion:
!
!    The input vector is (1,0,0,...,0) which should produce
!    the output (1,0,0,...,0).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 36

  real ( kind = 4 ) error
  real ( kind = 4 ) wsave(2*n+15)
  real ( kind = 4 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST11'
  write ( *, '(a)' ) '  RFFTB can recover a real vector of data from Fourier'
  write ( *, '(a)' ) '  coefficients.  In this case, the coefficients are:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    (1,0,0,...,0)'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  and the data should be:'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '    (1,1,1,...,1).'

  x(1) = 1.0E+00
  x(2:n) = 0.0E+00
!
!  Initialize the WSAVE array.
!
  call rffti ( n, wsave )
!
!  Compute the inverse Fourier transform.
!
  call rfftb ( n, x, wsave )
!
!  Compute the maximum error.
!
  error = maxval ( abs ( x(1:n) - 1.0E+00 ) )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  The maximum error in computation is ', error

  return
end
subroutine test12 ( )

!*****************************************************************************80
!
!! TEST12 tests R4_SFTB, R4_SFTF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2009
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
  write ( *, '(a)' ) 'TEST12'
  write ( *, '(a)' ) '  For real slow Fourier transform,'
  write ( *, '(a)' ) '  R4_SFTF computes the forward transform.'
  write ( *, '(a)' ) '  R4_SFTB computes the backward transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  The number of data values, N = ', n

  seed = 1973

  call r4vec_uniform ( n, alo, ahi, seed, x )

  call r4vec_print_some ( n, x, 10, '  The original data:' )
!
!  Compute the slow Fourier transform of the data.
!
  call r4_sftf ( n, x, azero, a, b )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  A (cosine) coefficients:'
  write ( *, '(a)' ) ' '

  write ( *, '(i3,g14.6)' ) 0, azero

  do i = 1, n/2
    write ( *, '(i3,g14.6)' ) i, a(i)
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  B (sine) coefficients:'
  write ( *, '(a)' ) ' '

  do i = 1, n/2
    write ( *, '(i3,g14.6)' ) i, b(i)
  end do
!
!  Now try to retrieve the data from the coefficients.
!
  call r4_sftb ( n, x, azero, a, b )

  call r4vec_print_some ( n, x, 10, '  The retrieved data:' )

  return
end
subroutine test13 ( )

!*****************************************************************************80
!
!! TEST13 tests SINQB, SINQF, SINQI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4096

  real ( kind = 4 ), parameter :: ahi = 5.0E+00
  real ( kind = 4 ), parameter :: alo = 0.0E+00
  integer ( kind = 4 ) seed
  real ( kind = 4 ) wsave(3*n+15)
  real ( kind = 4 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST13'
  write ( *, '(a)' ) '  For real fast sine quarter wave transform,'
  write ( *, '(a)' ) '  SINQI initializes the transform;'
  write ( *, '(a)' ) '  SINQF does a forward transform;'
  write ( *, '(a)' ) '  SINQB does a backward transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  The number of data items is N = ', n
!
!  Set the data values.
!
  seed = 1973

  call r4vec_uniform ( n, alo, ahi, seed, x )

  call r4vec_print_some ( n, x, 10, '  The original data:' )
!
!  Initialize the WSAVE array.
!
  call sinqi ( n, wsave )
!
!  Compute the coefficients.
!
  call sinqf ( n, x, wsave )

  call r4vec_print_some ( n, x, 10, '  The sine coefficients:' )
!
!  Now compute inverse transform of coefficients.  Should get back the
!  original data.

  call sinqb ( n, x, wsave )

  x(1:n) = x(1:n) / real ( 4 * n, kind = 4 )

  call r4vec_print_some ( n, x, 10, '  The retrieved data:' )

  return
end
subroutine test14 ( )

!*****************************************************************************80
!
!! TEST14 tests R4_SQSTB, R4_SQSTF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4096

  real ( kind = 4 ), parameter :: ahi = 5.0E+00
  real ( kind = 4 ), parameter :: alo = 0.0E+00
  integer ( kind = 4 ) seed
  real ( kind = 4 ) x(n)
  real ( kind = 4 ) y(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST14'
  write ( *, '(a)' ) '  For real slow quarter wave sine transform,'
  write ( *, '(a)' ) '  R4_SQSTF does a forward transform;'
  write ( *, '(a)' ) '  R4_SQSTB does a backward transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  The number of data items is N = ', n
!
!  Set the data values.
!
  seed = 1973

  call r4vec_uniform ( n, alo, ahi, seed, x )

  call r4vec_print_some ( n, x, 10, '  The original data:' )
!
!  Compute the coefficients.
!
  call r4_sqstf ( n, x, y )

  call r4vec_print_some ( n, y, 10, '  The sine coefficients:' )
!
!  Now compute inverse transform of coefficients.  Should get back the
!  original data.

  call r4_sqstb ( n, y, x )

  call r4vec_print_some ( n, x, 10, '  The retrieved data:' )

  return
end
subroutine test15 ( )

!*****************************************************************************80
!
!! TEST15 tests RSINT, RSINTI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4096

  real ( kind = 4 ), parameter :: ahi = 5.0E+00
  real ( kind = 4 ), parameter :: alo = 0.0E+00
  integer ( kind = 4 ) seed
  real ( kind = 4 ) wsave((5*n+30)/2)
  real ( kind = 4 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST15'
  write ( *, '(a)' ) '  For real fast sine transform,'
  write ( *, '(a)' ) '  RSINTI initializes the transform.'
  write ( *, '(a)' ) '  RSINT does a forward or backward transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  The number of data items is N = ', n
!
!  Set the data values.
!
  seed = 1973

  call r4vec_uniform ( n, alo, ahi, seed, x )

  call r4vec_print_some ( n, x, 10, '  The original data:' )
!
!  Initialize the WSAVE array.
!
  call rsinti ( n, wsave )
!
!  Compute the coefficients.
!
  call rsint ( n, x, wsave )

  call r4vec_print_some ( n, x, 10, '  The sine coefficients:' )
!
!  Now compute inverse transform of coefficients.  Should get back the
!  original data.

  call rsint ( n, x, wsave )

  x(1:n) = x(1:n) / real ( 2 * ( n + 1 ), kind = 4 )

  call r4vec_print_some ( n, x, 10, '  The retrieved data:' )

  return
end
subroutine test16 ( )

!*****************************************************************************80
!
!! TEST16 tests DSINT, DSINTI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4096

  real ( kind = 8 ), parameter :: ahi = 5.0D+00
  real ( kind = 8 ), parameter :: alo = 0.0D+00
  integer ( kind = 4 ) seed
  real ( kind = 8 ) wsave((5*n+30)/2)
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST16'
  write ( *, '(a)' ) '  For double precision fast sine transform,'
  write ( *, '(a)' ) '  DSINTI initializes the transform.'
  write ( *, '(a)' ) '  DSINT does a forward or backward transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  The number of data items is N = ', n
!
!  Set the data values.
!
  seed = 1973

  call r8vec_uniform ( n, alo, ahi, seed, x )

  call r8vec_print_some ( n, x, 10, '  The original data:' )
!
!  Initialize the WSAVE array.
!
  call dsinti ( n, wsave )
!
!  Compute the coefficients.
!
  call dsint ( n, x, wsave )

  call r8vec_print_some ( n, x, 10, '  The sine coefficients:' )
!
!  Now compute inverse transform of coefficients.  Should get back the
!  original data.

  call dsint ( n, x, wsave )

  x(1:n) = x(1:n) / real ( 2 * ( n + 1 ), kind = 8 )

  call r8vec_print_some ( n, x, 10, '  The retrieved data:' )

  return
end
subroutine test17 ( )

!*****************************************************************************80
!
!! TEST17 tests R4_SST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4096

  real ( kind = 4 ), parameter :: ahi = 5.0E+00
  real ( kind = 4 ), parameter :: alo = 0.0E+00
  real ( kind = 4 ) c(n)
  real ( kind = 4 ) d(n)
  real ( kind = 4 ) e(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST17'
  write ( *, '(a)' ) '  For real slow sine transform,'
  write ( *, '(a)' ) '  R4_SST does a forward or backward transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  The number of data items is N = ', n
!
!  Set the data values.
!
  seed = 1973

  call r4vec_uniform ( n, alo, ahi, seed, c )

  call r4vec_print_some ( n, c, 10, '  The original data:' )
!
!  Compute the coefficients.
!
  call r4_sst ( n, c, d )

  call r4vec_print_some ( n, d, 10, '  The sine coefficients:' )
!
!  Now compute inverse transform of coefficients.  Should get back the
!  original data.

  call r4_sst ( n, d, e )

  e(1:n) = e(1:n) / real ( 2 * ( n + 1 ), kind = 4 )

  call r4vec_print_some ( n, e, 10, '  The retrieved data:' )

  return
end
subroutine test18 ( )

!*****************************************************************************80
!
!! TEST18 tests R8_SST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4096

  real ( kind = 8 ), parameter :: ahi = 5.0D+00
  real ( kind = 8 ), parameter :: alo = 0.0D+00
  real ( kind = 8 ) c(n)
  real ( kind = 8 ) d(n)
  real ( kind = 8 ) e(n)
  integer seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST18'
  write ( *, '(a)' ) '  For double precision slow sine transform,'
  write ( *, '(a)' ) '  R8_SST does a forward or backward transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  The number of data items is N = ', n
!
!  Set the data values.
!
  seed = 1973

  call r8vec_uniform ( n, alo, ahi, seed, c )

  call r8vec_print_some ( n, c, 10, '  The original data:' )
!
!  Compute the coefficients.
!
  call r8_sst ( n, c, d )

  call r8vec_print_some ( n, d, 10, '  The sine coefficients:' )
!
!  Now compute inverse transform of coefficients.  Should get back the
!  original data.

  call r8_sst ( n, d, e )

  e(1:n) = e(1:n) / real ( 2 * ( n + 1 ), kind = 8 )

  call r8vec_print_some ( n, e, 10, '  The retrieved data:' )

  return
end
subroutine test19 ( )

!*****************************************************************************80
!
!! TEST19 tests RCOST, RCOSTI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4096

  real ( kind = 4 ), parameter :: ahi = 5.0E+00
  real ( kind = 4 ), parameter :: alo = 0.0E+00
  integer ( kind = 4 ) seed
  real ( kind = 4 ) wsave(3*n+15)
  real ( kind = 4 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST19'
  write ( *, '(a)' ) '  For real fast cosine transform,'
  write ( *, '(a)' ) '  RCOSTI initializes the transform.'
  write ( *, '(a)' ) '  RCOST does a forward or backward transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  The number of data items is N = ', n
!
!  Set the data values.
!
  seed = 1973

  call r4vec_uniform ( n, alo, ahi, seed, x )

  call r4vec_print_some ( n, x, 10, '  The original data:' )
!
!  Initialize the WSAVE array.
!
  call rcosti ( n, wsave )
!
!  Compute the coefficients.
!
  call rcost ( n, x, wsave )

  call r4vec_print_some ( n, x, 10, '  The cosine coefficients:' )
!
!  Now compute inverse transform of coefficients.  Should get back the
!  original data.
!
  call rcost ( n, x, wsave )

  x(1:n) = x(1:n) / real ( 2 * ( n - 1 ), kind = 4 )

  call r4vec_print_some ( n, x, 10, '  The retrieved data:' )

  return
end
subroutine test20 ( )

!*****************************************************************************80
!
!! TEST20 tests DCOST, DCOSTI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4096

  real ( kind = 8 ), parameter :: ahi = 5.0D+00
  real ( kind = 8 ), parameter :: alo = 0.0D+00
  integer ( kind = 4 ) seed
  real ( kind = 8 ) wsave(3*n+15)
  real ( kind = 8 ) x(n)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST20'
  write ( *, '(a)' ) '  For double precision fast cosine transform,'
  write ( *, '(a)' ) '  DCOSTI initializes the transform.'
  write ( *, '(a)' ) '  DCOST does a forward or backward transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  The number of data items is N = ', n
!
!  Set the data values.
!
  seed = 1973

  call r8vec_uniform ( n, alo, ahi, seed, x )

  call r8vec_print_some ( n, x, 10, '  The original data:' )
!
!  Initialize the WSAVE array.
!
  call dcosti ( n, wsave )
!
!  Compute the coefficients.
!
  call dcost ( n, x, wsave )

  call r8vec_print_some ( n, x, 10, '  The cosine coefficients:' )
!
!  Now compute inverse transform of coefficients.  Should get back the
!  original data.
!
  call dcost ( n, x, wsave )

  x(1:n) = x(1:n) / real ( 2 * ( n - 1 ), kind = 8 )

  call r8vec_print_some ( n, x, 10, '  The retrieved data:' )

  return
end
subroutine test21 ( )

!*****************************************************************************80
!
!! TEST21 tests R4_SCT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4096

  real ( kind = 4 ), parameter :: ahi = 5.0E+00
  real ( kind = 4 ), parameter :: alo = 0.0E+00
  real ( kind = 4 ) c(n)
  real ( kind = 4 ) d(n)
  real ( kind = 4 ) e(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST21'
  write ( *, '(a)' ) '  For real slow cosine transform,'
  write ( *, '(a)' ) '  R4_SCT does a forward or backward transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  The number of data items is N = ', n
!
!  Set the data values.
!
  seed = 1973

  call r4vec_uniform ( n, alo, ahi, seed, c )

  call r4vec_print_some ( n, c, 10, '  The original data:' )
!
!  Compute the coefficients.
!
  call r4_sct ( n, c, d )

  call r4vec_print_some ( n, d, 10, '  The cosine coefficients:' )
!
!  Now compute inverse transform of coefficients.  Should get back the
!  original data.

  call r4_sct ( n, d, e )

  e(1:n) = e(1:n) / real ( 2 * ( n + 1 ), kind = 4 )

  call r4vec_print_some ( n, e, 10, '  The retrieved data:' )

  return
end
subroutine test22 ( )

!*****************************************************************************80
!
!! TEST22 tests R8_SCT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 4096

  real ( kind = 8 ), parameter :: ahi = 5.0D+00
  real ( kind = 8 ), parameter :: alo = 0.0D+00
  real ( kind = 8 ) c(n)
  real ( kind = 8 ) d(n)
  real ( kind = 8 ) e(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST22'
  write ( *, '(a)' ) '  For double precision slow cosine transform,'
  write ( *, '(a)' ) '  R8_SCT does a forward or backward transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  The number of data items is N = ', n
!
!  Set the data values.
!
  seed = 1973

  call r8vec_uniform ( n, alo, ahi, seed, c )

  call r8vec_print_some ( n, c, 10, '  The original data:' )
!
!  Compute the coefficients.
!
  call r8_sct ( n, c, d )

  call r8vec_print_some ( n, d, 10, '  The cosine coefficients:' )
!
!  Now compute inverse transform of coefficients.  Should get back the
!  original data.

  call r8_sct ( n, d, e )

  e(1:n) = e(1:n) / real ( 2 * n, kind = 8 )

  call r8vec_print_some ( n, e, 10, '  The retrieved data:' )

  return
end
subroutine test23 ( )

!*****************************************************************************80
!
!! TEST23 tests R4_SHT.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2009
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ), parameter :: n = 17

  real ( kind = 4 ), parameter :: ahi = 5.0E+00
  real ( kind = 4 ), parameter :: alo = 0.0E+00
  real ( kind = 4 ) c(n)
  real ( kind = 4 ) d(n)
  real ( kind = 4 ) e(n)
  integer ( kind = 4 ) seed

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST23'
  write ( *, '(a)' ) '  For real slow Hartley transform,'
  write ( *, '(a)' ) '  R4_SHT does a forward or backward transform.'
  write ( *, '(a)' ) ' '
  write ( *, '(a,i4)' ) '  The number of data items is N = ', n
!
!  Set the data values.
!
  seed = 1973

  call r4vec_uniform ( n, alo, ahi, seed, c )

  call r4vec_print_some ( n, c, 10, '  The original data:' )
!
!  Compute the coefficients.
!
  call r4_sht ( n, c, d )

  call r4vec_print_some ( n, d, 10, '  The Hartley coefficients:' )
!
!  Now compute inverse transform of coefficients.  Should get back the
!  original data.

  call r4_sht ( n, d, e )

  call r4vec_print_some ( n, e, 10, '  The retrieved data:' )

  return
end
