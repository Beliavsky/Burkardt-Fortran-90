subroutine c4vec_print_some ( n, x, max_print, title )

!*****************************************************************************80
!
!! C4VEC_PRINT_SOME prints some of a C4VEC.
!
!  Discussion:
!
!    The user specifies MAX_PRINT, the maximum number of lines to print.
!
!    If N, the size of the vector, is no more than MAX_PRINT, then
!    the entire vector is printed, one entry per line.
!
!    Otherwise, if possible, the first MAX_PRINT-2 entries are printed,
!    followed by a line of periods suggesting an omission,
!    and the last entry.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 December 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries of the vector.
!
!    Input, complex ( kind = 4 ) X(N), the vector to be printed.
!
!    Input, integer ( kind = 4 ) MAX_PRINT, the maximum number of lines
!    to print.
!
!    Input, character ( len = * ) TITLE, an optional title.
!
  implicit none

  integer   ( kind = 4 ) n

  integer   ( kind = 4 ) i
  integer   ( kind = 4 ) max_print
  character ( len = * )  title
  complex   ( kind = 4 ) x(n)

  if ( max_print <= 0 ) then
    return
  end if

  if ( n <= 0 ) then
    return
  end if

  if ( 0 < len_trim ( title ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )
    write ( *, '(a)' ) ' '
  end if

  if ( n <= max_print ) then

    do i = 1, n
      write ( *, '(i8,2x,2g14.6)' ) i, x(i)
    end do

  else if ( 3 <= max_print ) then

    do i = 1, max_print-2
      write ( *, '(i8,2x,2g14.6)' ) i, x(i)
    end do
    write ( *, '(a)' ) '......  ..............'
    i = n
    write ( *, '(i8,2x,2g14.6)' ) i, x(i)

  else

    do i = 1, max_print - 1
      write ( *, '(i8,2x,2g14.6)' ) i, x(i)
    end do
    i = max_print
    write ( *, '(i8,2x,2g14.6,2x,a)' ) i, x(i), '...more entries...'

  end if

  return
end
subroutine c4vec_uniform_01 ( n, seed, c )

!*****************************************************************************80
!
!! C4VEC_UNIFORM_01 returns a unit pseudorandom C4VEC.
!
!  Discussion:
!
!    A C4VEC is a vector of complex ( kind = 4 ) values.
!
!    The angles should be uniformly distributed between 0 and 2 * PI,
!    the square roots of the radius uniformly distributed between 0 and 1.
!
!    This results in a uniform distribution of values in the unit circle.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 March 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of values to compute.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, complex ( kind = 4 ) C(N), the pseudorandom complex vector.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 4 ) c(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  real    ( kind = 4 ), parameter :: pi = 3.1415926E+00
  real    ( kind = 4 ) r
  integer ( kind = 4 ) seed
  real    ( kind = 4 ) theta

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'C4VEC_UNIFORM_01 - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop
  end if

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + 2147483647
    end if

    r = sqrt (  real ( seed, kind = 4 ) * 4.656612875E-10 )

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + 2147483647
    end if

    theta = 2.0E+00 * pi * ( real ( seed, kind = 4 ) * 4.656612875E-10 )

    c(i) = r * cmplx ( cos ( theta ), sin ( theta ), kind = 4 )

  end do

  return
end
subroutine cfftb ( n, c, wsave )

!*****************************************************************************80
!
!! CFFTB computes the backward complex discrete Fourier transform.
!
!  Discussion:
!
!    This process is sometimes called Fourier synthesis.
!
!    CFFTB computes a complex periodic sequence from its Fourier coefficients.
!
!    A call of CFFTF followed by a call of CFFTB will multiply the
!    sequence by N.  In other words, the transforms are not normalized.
!
!    The array WSAVE must be initialized by CFFTI.
!
!    The transform is defined by:
!
!      C_out(J) = sum ( 1 <= K <= N )
!        C_in(K) * exp ( sqrt ( - 1 ) * ( J - 1 ) * ( K - 1 ) * 2 * PI / N )
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    Paul Swarztrauber,
!    Vectorizing the FFT's,
!    in Parallel Computations,
!    G. Rodrigue, editor,
!    Academic Press, 1982, pages 51-83.
!
!    Bill Buzbee,
!    The SLATEC Common Math Library,
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the sequence to be transformed.
!    The method is more efficient when N is the product of small primes.
!
!    Input/output, complex ( kind = 4 ) C(N).
!    On input, C contains the sequence of Fourier coefficients.
!    On output, C contains the sequence of data values that correspond
!    to the input coefficients.
!
!    Input, real ( kind = 4 ) WSAVE(4*N+15).  The array must be initialized
!    by calling CFFTI.  A different WSAVE array must be used for each different
!    value of N.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 4 ) c(n)
  real    ( kind = 4 ) wsave(4*n+15)

  if ( n <= 1 ) then
    return
  end if

  call cfftb1 ( n, c, wsave(1), wsave(2*n+1), wsave(4*n+1) )

  return
end
subroutine cfftb1 ( n, c, ch, wa, ifac )

!*****************************************************************************80
!
!! CFFTB1 is a lower-level routine used by CFFTB.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    12 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the sequence to be transformed.
!
!    Input/output, complex ( kind = 4 ) C(N).
!    On input, C contains the sequence of Fourier coefficients.
!    On output, C contains the sequence of data values that correspond
!    to the input coefficients.
!
!    Input, complex ( kind = 4 ) CH(N).
!
!    Input, real ( kind = 4 ) WA(2*N).
!
!    Input, integer ( kind = 4 ) IFAC(15).
!    IFAC(1) = N, the number that was factored.
!    IFAC(2) = NF, the number of factors.
!    IFAC(3:2+NF), the factors.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 4 ) c(n)
  complex ( kind = 4 ) ch(n)
  integer ( kind = 4 ) idl1
  integer ( kind = 4 ) ido
  integer ( kind = 4 ) ifac(15)
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) iw
  integer ( kind = 4 ) ix2
  integer ( kind = 4 ) ix3
  integer ( kind = 4 ) ix4
  integer ( kind = 4 ) k1
  integer ( kind = 4 ) l1
  integer ( kind = 4 ) l2
  integer ( kind = 4 ) na
  integer ( kind = 4 ) nac
  integer ( kind = 4 ) nf
  real    ( kind = 4 ) wa(2*n)

  nf = ifac(2)
  na = 0
  l1 = 1
  iw = 1

  do k1 = 1, nf

    ip = ifac(k1+2)
    l2 = ip * l1
    ido = n / l2
    idl1 = 2 * ido * l1

    if ( ip == 4 ) then

      ix2 = iw + 2 * ido
      ix3 = ix2 + 2 * ido

      if ( na == 0 ) then
        call passb4 ( 2*ido, l1, c, ch, wa(iw), wa(ix2), wa(ix3) )
      else
        call passb4 ( 2*ido, l1, ch, c, wa(iw), wa(ix2), wa(ix3) )
      end if

      na = 1 - na

    else if ( ip == 2 ) then

      if ( na == 0 ) then
        call passb2 ( 2*ido, l1, c, ch, wa(iw) )
      else
        call passb2 ( 2*ido, l1, ch, c, wa(iw) )
      end if

      na = 1 - na

    else if ( ip == 3 ) then

      ix2 = iw + 2 * ido

      if ( na == 0 ) then
        call passb3 ( 2*ido, l1, c, ch, wa(iw), wa(ix2) )
      else
        call passb3 ( 2*ido, l1, ch, c, wa(iw), wa(ix2) )
      end if

      na = 1 - na

    else if ( ip == 5 ) then

      ix2 = iw + 2 * ido
      ix3 = ix2 + 2 * ido
      ix4 = ix3 + 2 * ido

      if ( na == 0 ) then
        call passb5 ( 2*ido, l1, c, ch, wa(iw), wa(ix2), wa(ix3), wa(ix4) )
      else
        call passb5 ( 2*ido, l1, ch, c, wa(iw), wa(ix2), wa(ix3), wa(ix4) )
      end if

      na = 1 - na

    else

      if ( na == 0 ) then
        call passb ( nac, 2*ido, ip, l1, idl1, c, c, c, ch, ch, wa(iw) )
      else
        call passb ( nac, 2*ido, ip, l1, idl1, ch, ch, ch, c, c, wa(iw) )
      end if

      if ( nac /= 0 ) then
        na = 1 - na
      end if

    end if

    l1 = l2
    iw = iw + ( ip - 1 ) * 2 * ido

  end do

  if ( na /= 0 ) then
    c(1:n) = ch(1:n)
  end if

  return
end
subroutine cfftb_2d ( ldf, n, f, wsave )

!*****************************************************************************80
!
!! CFFTB_2D computes a backward two dimensional complex fast Fourier transform.
!
!  Discussion:
!
!    The routine computes the backward two dimensional fast Fourier transform,
!    of a complex N by N matrix of data.
!
!    The output is unscaled, that is, a call to CFFTB_2D followed by a call
!    to CFFTF_2D will return the original data multiplied by N*N.
!
!    For some applications it is desirable to have the transform scaled so
!    the center of the N by N frequency square corresponds to zero
!    frequency.  The user can do this replacing the original input data
!    F(I,J) by F(I,J) * (-1.)**(I+J),  I,J =0,...,N-1.
!
!    Before calling CFFTF_2D or CFFTB_2D, it is necessary to initialize
!    the array WSAVE by calling CFFTI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) LDF, the leading dimension of the matrix.
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns in
!    the matrix.
!
!    Input/output, complex ( kind = 4 ) F(LDF,N),
!    On input, an N by N array of complex values to be transformed.
!    On output, the transformed values.
!
!    Input, real ( kind = 4 ) WSAVE(4*N+15), a work array whose values
!    depend on N, and which must be initialized by calling CFFTI.
!
  implicit none

  integer ( kind = 4 ) ldf
  integer ( kind = 4 ) n

  complex ( kind = 4 ) f(ldf,n)
  integer ( kind = 4 ) i
  real    ( kind = 4 ) wsave(4*n+15)
!
!  Row transforms:
!
  f(1:n,1:n) = transpose ( f(1:n,1:n) )

  do i = 1, n
    call cfftb ( n, f(1,i), wsave )
  end do

  f(1:n,1:n) = transpose ( f(1:n,1:n) )
!
!  Column transforms:
!
  do i = 1, n
    call cfftb ( n, f(1,i), wsave )
  end do

  return
end
subroutine cfftf ( n, c, wsave )

!*****************************************************************************80
!
!! CFFTF computes the forward complex discrete Fourier transform.
!
!  Discussion:
!
!    This process is sometimes called Fourier analysis.
!
!    CFFTF computes the Fourier coefficients of a complex periodic sequence.
!
!    The transform is not normalized.  To obtain a normalized transform,
!    the output must be divided by N.  Otherwise a call of CFFTF
!    followed by a call of CFFTB will multiply the sequence by N.
!
!    The array WSAVE must be initialized by calling CFFTI.
!
!    The transform is defined by:
!
!      C_out(J) = sum ( 1 <= K <= N )
!        C_in(K) * exp ( - sqrt ( -1 ) * ( J - 1 ) * ( K - 1 ) * 2 * PI / N )
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    Paul Swarztrauber,
!    Vectorizing the FFT's,
!    in Parallel Computations,
!    G. Rodrigue, editor,
!    Academic Press, 1982, pages 51-83.
!
!    Bill Buzbee,
!    The SLATEC Common Math Library,
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the sequence to be transformed.
!    The method is more efficient when N is the product of small primes.
!
!    Input/output, complex ( kind = 4 ) C(N).
!    On input, the data sequence to be transformed.
!    On output, the Fourier coefficients.
!
!    Input, real ( kind = 4 ) WSAVE(4*N+15).  The array must be initialized
!    by calling CFFTI.  A different WSAVE array must be used for each different
!    value of N.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 4 ) c(n)
  real    ( kind = 4 ) wsave(4*n+15)

  if ( n <= 1 ) then
    return
  end if

  call cfftf1 ( n, c, wsave(1), wsave(2*n+1), wsave(4*n+1) )

  return
end
subroutine cfftf1 ( n, c, ch, wa, ifac )

!*****************************************************************************80
!
!! CFFTF1 is a lower level routine used by CFFTF.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the sequence to be transformed.
!
!    Input/output, complex ( kind = 4 ) C(N).
!    On input, the data sequence to be transformed.
!    On output, the Fourier coefficients.
!
!    Input, complex ( kind = 4 ) CH(N).
!
!    Input, real ( kind = 4 ) WA(2*N).
!
!    Input, integer ( kind = 4 ) IFAC(15).
!    IFAC(1) = N, the number that was factored.
!    IFAC(2) = NF, the number of factors.
!    IFAC(3:2+NF), the factors.
!
  implicit none

  integer ( kind = 4 ) n

  complex ( kind = 4 ) c(n)
  complex ( kind = 4 ) ch(n)
  integer ( kind = 4 ) idl1
  integer ( kind = 4 ) ido
  integer ( kind = 4 ) ifac(15)
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) iw
  integer ( kind = 4 ) ix2
  integer ( kind = 4 ) ix3
  integer ( kind = 4 ) ix4
  integer ( kind = 4 ) k1
  integer ( kind = 4 ) l1
  integer ( kind = 4 ) l2
  integer ( kind = 4 ) na
  integer ( kind = 4 ) nac
  integer ( kind = 4 ) nf
  real    ( kind = 4 ) wa(2*n)

  nf = ifac(2)
  na = 0
  l1 = 1
  iw = 1

  do k1 = 1, nf

    ip = ifac(k1+2)
    l2 = ip * l1
    ido = n / l2
    idl1 = 2 * ido * l1

    if ( ip == 4 ) then

      ix2 = iw + 2 * ido
      ix3 = ix2 + 2 * ido

      if ( na == 0 ) then
        call passf4 ( 2*ido, l1, c, ch, wa(iw), wa(ix2), wa(ix3) )
      else
        call passf4 ( 2*ido, l1, ch, c, wa(iw), wa(ix2), wa(ix3) )
      end if

      na = 1 - na

    else if ( ip == 2 ) then

      if ( na == 0 ) then
        call passf2 ( 2*ido, l1, c, ch, wa(iw) )
      else
        call passf2 ( 2*ido, l1, ch, c, wa(iw) )
      end if

      na = 1 - na

    else if ( ip == 3 ) then

      ix2 = iw + 2 * ido

      if ( na == 0 ) then
        call passf3 ( 2*ido, l1, c, ch, wa(iw), wa(ix2) )
      else
        call passf3 ( 2*ido, l1, ch, c, wa(iw), wa(ix2) )
      end if

      na = 1 - na

    else if ( ip == 5 ) then

      ix2 = iw + 2 * ido
      ix3 = ix2 + 2 * ido
      ix4 = ix3 + 2 * ido

      if ( na == 0 ) then
        call passf5 ( 2*ido, l1, c, ch, wa(iw), wa(ix2), wa(ix3), wa(ix4) )
      else
        call passf5 ( 2*ido, l1, ch, c, wa(iw), wa(ix2), wa(ix3), wa(ix4) )
      end if

      na = 1 - na

    else

      if ( na == 0 ) then
        call passf ( nac, 2*ido, ip, l1, idl1, c, c, c, ch, ch, wa(iw) )
      else
        call passf ( nac, 2*ido, ip, l1, idl1, ch, ch, ch, c, c, wa(iw) )
      end if

      if ( nac /= 0 ) then
        na = 1 - na
      end if

    end if

    l1 = l2
    iw = iw + ( ip - 1 ) * 2 * ido

  end do

  if ( na /= 0 ) then
    c(1:n) = ch(1:n)
  end if

  return
end
subroutine cfftf_2d ( ldf, n, f, wsave )

!*****************************************************************************80
!
!! CFFTF_2D computes a two dimensional complex fast Fourier transform.
!
!  Discussion:
!
!    The routine computes the forward two dimensional fast Fourier transform,
!    of a complex N by N matrix of data.
!
!    The output is unscaled, that is, a call to CFFTF_2D,
!    followed by a call to CFFTB_2D will return the original data
!    multiplied by N*N.
!
!    For some applications it is desirable to have the transform scaled so
!    the center of the N by N frequency square corresponds to zero
!    frequency.  The user can do this replacing the original input data
!    F(I,J) by F(I,J) *(-1.)**(I+J),  I,J =0,...,N-1.
!
!    Before calling CFFTF_2D or CFFTB_2D, it is necessary to initialize
!    the array WSAVE by calling CFFTI.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) LDF, the leading dimension of the matrix.
!
!    Input, integer ( kind = 4 ) N, the number of rows and columns in the matrix.
!
!    Input/output, complex ( kind = 4 ) F(LDF,N),
!    On input, an N by N array of complex values to be transformed.
!    On output, the transformed values.
!
!    Input, real ( kind = 4 ) WSAVE(4*N+15), a work array whose values depend on N,
!    and which must be initialized by calling CFFTI.
!
  implicit none

  integer ( kind = 4 ) ldf
  integer ( kind = 4 ) n

  complex ( kind = 4 ) f(ldf,n)
  integer ( kind = 4 ) i
  real    ( kind = 4 ) wsave(4*n+15)
!
!  Row transforms:
!
  f(1:n,1:n) = transpose ( f(1:n,1:n) )

  do i = 1, n
    call cfftf ( n, f(1,i), wsave )
  end do

  f(1:n,1:n) = transpose ( f(1:n,1:n) )
!
!  Column transforms:
!
  do i = 1, n
    call cfftf ( n, f(1,i), wsave )
  end do

  return
end
subroutine cffti ( n, wsave )

!*****************************************************************************80
!
!! CFFTI initializes WSAVE, used in CFFTF and CFFTB.
!
!  Discussion:
!
!    The prime factorization of N together with a tabulation of the
!    trigonometric functions are computed and stored in WSAVE.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    Paul Swarztrauber,
!    Vectorizing the FFT's,
!    in Parallel Computations,
!    G. Rodrigue, editor,
!    Academic Press, 1982, pages 51-83.
!
!    Bill Buzbee,
!    The SLATEC Common Math Library,
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the sequence to be transformed.
!
!    Output, real ( kind = 4 ) WSAVE(4*N+15), contains data, dependent on the value
!    of N, which is necessary for the CFFTF or CFFTB routines.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 4 ) wsave(4*n+15)

  if ( n <= 1 ) then
    return
  end if

  call cffti1 ( n, wsave(2*n+1), wsave(4*n+1) )

  return
end
subroutine cffti1 ( n, wa, ifac )

!*****************************************************************************80
!
!! CFFTI1 is a lower level routine used by CFFTI.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the sequence to be transformed.
!
!    Input, real ( kind = 4 ) WA(2*N).
!
!    Input, integer ( kind = 4 ) IFAC(15).
!    IFAC(1) = N, the number that was factored.
!    IFAC(2) = NF, the number of factors.
!    IFAC(3:2+NF), the factors.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 4 ) arg
  real    ( kind = 4 ) argh
  real    ( kind = 4 ) argld
  real    ( kind = 4 ) fi
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i1
  integer ( kind = 4 ) ido
  integer ( kind = 4 ) ifac(15)
  integer ( kind = 4 ) ii
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k1
  integer ( kind = 4 ) l1
  integer ( kind = 4 ) l2
  integer ( kind = 4 ) ld
  integer ( kind = 4 ) nf
  real    ( kind = 4 ), parameter :: pi = 3.1415926E+00
  real    ( kind = 4 ) wa(2*n)

  call i4_factor ( n, ifac )

  nf = ifac(2)

  argh = 2.0E+00 * pi / real ( n, kind = 4 )
  i = 2
  l1 = 1

  do k1 = 1, nf

    ip = ifac(k1+2)
    ld = 0
    l2 = l1 * ip
    ido = n / l2

    do j = 1, ip-1

      i1 = i
      wa(i-1) = 1.0E+00
      wa(i) = 0.0E+00
      ld = ld + l1
      fi = 0.0E+00
      argld = real ( ld, kind = 4 ) * argh

      do ii = 4, 2*ido+2, 2
        i = i + 2
        fi = fi + 1.0E+00
        arg = fi * argld
        wa(i-1) = cos ( arg )
        wa(i) = sin ( arg )
      end do

      if ( 5 < ip ) then
        wa(i1-1) = wa(i-1)
        wa(i1) = wa(i)
      end if

    end do

    l1 = l2

  end do

  return
end
subroutine cosqb ( n, x, wsave )

!*****************************************************************************80
!
!! COSQB computes the fast cosine transform of quarter wave data.
!
!  Discussion:
!
!    COSQB computes a sequence from its representation in terms of a cosine
!    series with odd wave numbers.
!
!    The transform is defined by:
!
!      X_out(I) = sum ( 1 <= K <= N )
!
!        4 * X_in(K) * cos ( ( 2 * K - 1 ) * ( I - 1 ) * PI / ( 2 * N ) )
!
!    COSQB is the unnormalized inverse of COSQF since a call of COSQB
!    followed by a call of COSQF will multiply the input sequence X by 4*N.
!
!    The array WSAVE must be initialized by calling COSQI.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    Paul Swarztrauber,
!    Vectorizing the FFT's,
!    in Parallel Computations,
!    G. Rodrigue, editor,
!    Academic Press, 1982, pages 51-83.
!
!    Bill Buzbee,
!    The SLATEC Common Math Library,
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the array X.  The method is
!    more efficient when N is the product of small primes.
!
!    Input/output, real ( kind = 4 ) X(N).
!    On input, the cosine series coefficients.
!    On output, the corresponding data vector.
!
!    Input, real ( kind = 4 ) WSAVE(3*N+15), contains data, depending on N, and
!    required by the algorithm.  The WSAVE array must be initialized by
!    calling COSQI.  A different WSAVE array must be used for each different
!    value of N.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 4 ), parameter :: tsqrt2 = 2.82842712474619E+00
  real    ( kind = 4 ) wsave(3*n+15)
  real    ( kind = 4 ) x(n)
  real    ( kind = 4 ) x1

  if ( n < 2 ) then
    x(1) = 4.0E+00 * x(1)
  else if ( n == 2 ) then
    x1 = 4.0E+00 * ( x(1) + x(2) )
    x(2) = tsqrt2 * ( x(1) - x(2) )
    x(1) = x1
  else
    call cosqb1 ( n, x, wsave(1), wsave(n+1) )
  end if

  return
end
subroutine cosqb1 ( n, x, w, xh )

!*****************************************************************************80
!
!! COSQB1 is a lower level routine used by COSQB.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the array.
!
!    Input/output, real ( kind = 4 ) X(N).
!    On input, the cosine series coefficients.
!    On output, the corresponding data vector.
!
!    Input, real ( kind = 4 ) W(N).
!
!    Input, real ( kind = 4 ) XH(2*N+15).
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kc
  integer ( kind = 4 ) ns2
  real    ( kind = 4 ) w(n)
  real    ( kind = 4 ) x(n)
  real    ( kind = 4 ) xh(2*n+15)
  real    ( kind = 4 ) xim1

  ns2 = ( n + 1 ) / 2

  do i = 3, n, 2
    xim1 = x(i-1) + x(i)
    x(i) = x(i) - x(i-1)
    x(i-1) = xim1
  end do

  x(1) = x(1) + x(1)

  if ( mod ( n, 2 ) == 0 ) then
    x(n) = 2.0E+00 * x(n)
  end if

  call rfftb ( n, x, xh )

  do k = 2, ns2
    kc = n + 2 - k
    xh(k) = w(k-1) * x(kc) + w(kc-1) * x(k)
    xh(kc) = w(k-1) * x(k) - w(kc-1) * x(kc)
  end do

  if ( mod ( n, 2 ) == 0 ) then
    x(ns2+1) = w(ns2) * ( x(ns2+1) + x(ns2+1) )
  end if

  do k = 2, ns2
    kc = n + 2 - k
    x(k) = xh(k) + xh(kc)
    x(kc) = xh(k) - xh(kc)
  end do

  x(1) = 2.0E+00 * x(1)

  return
end
subroutine cosqf ( n, x, wsave )

!*****************************************************************************80
!
!! COSQF computes the fast cosine transform of quarter wave data.
!
!  Discussion:
!
!    COSQF computes the coefficients in a cosine series representation
!    with only odd wave numbers.
!
!    COSQF is the unnormalized inverse of COSQB since a call of COSQF
!    followed by a call of COSQB will multiply the input sequence X
!    by 4*N.
!
!    The array WSAVE must be initialized by calling COSQI.
!
!    The transform is defined by:
!
!      X_out(I) = X_in(1) + sum ( 2 <= K <= N )
!
!        2 * X_in(K) * cos ( ( 2 * I - 1 ) * ( K - 1 ) * PI / ( 2 * N ) )
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    Paul Swarztrauber,
!    Vectorizing the FFT's,
!    in Parallel Computations,
!    G. Rodrigue, editor,
!    Academic Press, 1982, pages 51-83.
!
!    Bill Buzbee,
!    The SLATEC Common Math Library,
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the array X.  The method is
!    more efficient when N is the product of small primes.
!
!    Input/output, real ( kind = 4 ) X(N).
!    On input, the data to be transformed.
!    On output, the transformed data.
!
!    Input, real ( kind = 4 ) WSAVE(3*N+15), contains data, depending on N, and
!    required by the algorithm.  The WSAVE array must be initialized by
!    calling COSQI.  A different WSAVE array must be used for each different
!    value of N.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 4 ), parameter :: sqrt2 = 1.4142135623731E+00
  real    ( kind = 4 ) tsqx
  real    ( kind = 4 ) wsave(3*n+15)
  real    ( kind = 4 ) x(n)

  if ( n < 2 ) then

  else if ( n == 2 ) then
    tsqx = sqrt2 * x(2)
    x(2) = x(1) - tsqx
    x(1) = x(1) + tsqx
  else
    call cosqf1 ( n, x, wsave(1), wsave(n+1) )
  end if

  return
end
subroutine cosqf1 ( n, x, w, xh )

!*****************************************************************************80
!
!! COSQF1 is a lower level routine used by COSQF.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the array to be transformed.
!
!    Input/output, real ( kind = 4 ) X(N).
!    On input, the data to be transformed.
!    On output, the transformed data.
!
!    Input, real ( kind = 4 ) W(N).
!
!    Input, real ( kind = 4 ) XH(2*N+15).
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kc
  integer ( kind = 4 ) ns2
  real    ( kind = 4 ) w(n)
  real    ( kind = 4 ) x(n)
  real    ( kind = 4 ) xh(2*n+15)
  real    ( kind = 4 ) xim1

  ns2 = ( n + 1 ) / 2

  do k = 2, ns2
    kc = n + 2 - k
    xh(k) = x(k) + x(kc)
    xh(kc) = x(k) - x(kc)
  end do

  if ( mod ( n, 2 ) == 0 ) then
    xh(ns2+1) = x(ns2+1) + x(ns2+1)
  end if

  do k = 2, ns2
    kc = n+2-k
    x(k) = w(k-1) * xh(kc) + w(kc-1) * xh(k)
    x(kc) = w(k-1) * xh(k) - w(kc-1) * xh(kc)
  end do

  if ( mod ( n, 2 ) == 0 ) then
    x(ns2+1) = w(ns2) * xh(ns2+1)
  end if

  call rfftf ( n, x, xh )

  do i = 3, n, 2
    xim1 = x(i-1) - x(i)
    x(i) = x(i-1) + x(i)
    x(i-1) = xim1
  end do

  return
end
subroutine cosqi ( n, wsave )

!*****************************************************************************80
!
!! COSQI initializes WSAVE, used in COSQF and COSQB.
!
!  Discussion:
!
!    The prime factorization of N together with a tabulation of the
!    trigonometric functions are computed and stored in WSAVE.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    Paul Swarztrauber,
!    Vectorizing the FFT's,
!    in Parallel Computations,
!    G. Rodrigue, editor,
!    Academic Press, 1982, pages 51-83.
!
!    Bill Buzbee,
!    The SLATEC Common Math Library,
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the array to be transformed.
!    The method is more efficient when N is the product of small primes.
!
!    Output, real ( kind = 4 ) WSAVE(3*N+15), contains data, depending on N,
!    and required by the COSQB and COSQF algorithms.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 4 ) dt
  integer ( kind = 4 ) k
  real    ( kind = 4 ), parameter :: pi = 3.1415926E+00
  real    ( kind = 4 ) wsave(3*n+15)

  dt = 0.5E+00 * pi / real ( n, kind = 4 )

  do k = 1, n
    wsave(k) = cos ( real ( k, kind = 4 ) * dt )
  end do

  call rffti ( n, wsave(n+1) )

  return
end
subroutine dadf2 ( ido, l1, cc, ch, wa1 )

!*****************************************************************************80
!
!! DADF2 is a lower level routine used by DFFTF1.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    07 January 2002
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) ido
  integer ( kind = 4 ) l1

  real    ( kind = 8 ) cc(ido,l1,2)
  real    ( kind = 8 ) ch(ido,2,l1)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ic
  integer ( kind = 4 ) k
  real    ( kind = 8 ) ti2
  real    ( kind = 8 ) tr2
  real    ( kind = 8 ) wa1(ido)

  ch(1,1,1:l1)   = cc(1,1:l1,1) + cc(1,1:l1,2)
  ch(ido,2,1:l1) = cc(1,1:l1,1) - cc(1,1:l1,2)

  if ( ido < 2 ) then
    return
  end if

  if ( 2 < ido ) then

    do k = 1, l1
      do i = 3, ido, 2

        ic = ido + 2 - i

        tr2 = wa1(i-2) * cc(i-1,k,2) + wa1(i-1) * cc(i,k,2)
        ti2 = wa1(i-2) * cc(i,k,2)   - wa1(i-1) * cc(i-1,k,2)

        ch(i,1,k) = cc(i,k,1) + ti2
        ch(ic,2,k) = ti2 - cc(i,k,1)
        ch(i-1,1,k) = cc(i-1,k,1) + tr2
        ch(ic-1,2,k) = cc(i-1,k,1) - tr2

      end do
    end do

    if ( mod ( ido, 2 ) == 1 ) then
      return
    end if

  end if

  ch(1,2,1:l1)   = - cc(ido,1:l1,2)
  ch(ido,1,1:l1) =   cc(ido,1:l1,1)

  return
end
subroutine dadf3 ( ido, l1, cc, ch, wa1, wa2 )

!*****************************************************************************80
!
!! DADF3 is a lower level routine used by DFFTF1.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    07 January 2002
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) ido
  integer ( kind = 4 ) l1

  real    ( kind = 8 ) cc(ido,l1,3)
  real    ( kind = 8 ) ch(ido,3,l1)
  real    ( kind = 8 ) ci2
  real    ( kind = 8 ) cr2
  real    ( kind = 8 ) di2
  real    ( kind = 8 ) di3
  real    ( kind = 8 ) dr2
  real    ( kind = 8 ) dr3
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ic
  integer ( kind = 4 ) k
  real    ( kind = 8 ) taui
  real    ( kind = 8 ), parameter :: taur = -0.5D+00
  real    ( kind = 8 ) ti2
  real    ( kind = 8 ) ti3
  real    ( kind = 8 ) tr2
  real    ( kind = 8 ) tr3
  real    ( kind = 8 ) wa1(ido)
  real    ( kind = 8 ) wa2(ido)

  taui = sqrt ( 3.0D+00 ) / 2.0D+00

  do k = 1, l1
    cr2 = cc(1,k,2) + cc(1,k,3)
    ch(1,1,k) = cc(1,k,1) + cr2
    ch(1,3,k) = taui * ( cc(1,k,3) - cc(1,k,2) )
    ch(ido,2,k) = cc(1,k,1) + taur * cr2
  end do

  if ( ido == 1 ) then
    return
  end if

  do k = 1, l1
    do i = 3, ido, 2

      ic = ido + 2 - i

      dr2 = wa1(i-2) * cc(i-1,k,2) + wa1(i-1) * cc(i,k,2)
      di2 = wa1(i-2) * cc(i,k,2)   - wa1(i-1) * cc(i-1,k,2)
      dr3 = wa2(i-2) * cc(i-1,k,3) + wa2(i-1) * cc(i,k,3)
      di3 = wa2(i-2) * cc(i,k,3)   - wa2(i-1) * cc(i-1,k,3)

      cr2 = dr2 + dr3
      ci2 = di2 + di3

      ch(i-1,1,k) = cc(i-1,k,1) + cr2
      ch(i,1,k)   = cc(i,k,1) + ci2

      tr2 = cc(i-1,k,1) + taur * cr2
      ti2 = cc(i,k,1) + taur * ci2
      tr3 = taui * ( di2 - di3 )
      ti3 = taui * ( dr3 - dr2 )

      ch(i-1,3,k) = tr2 + tr3
      ch(ic-1,2,k) = tr2 - tr3
      ch(i,3,k) = ti2 + ti3
      ch(ic,2,k) = ti3 - ti2

    end do
  end do

  return
end
subroutine dadf4 ( ido, l1, cc, ch, wa1, wa2, wa3 )

!*****************************************************************************80
!
!! DADF4 is a lower level routine used by DFFTF1.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    07 January 2002
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) ido
  integer ( kind = 4 ) l1

  real    ( kind = 8 ) cc(ido,l1,4)
  real    ( kind = 8 ) ch(ido,4,l1)
  real    ( kind = 8 ) ci2
  real    ( kind = 8 ) ci3
  real    ( kind = 8 ) ci4
  real    ( kind = 8 ) cr2
  real    ( kind = 8 ) cr3
  real    ( kind = 8 ) cr4
  real    ( kind = 8 ) hsqt2
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ic
  integer ( kind = 4 ) k
  real    ( kind = 8 ) ti1
  real    ( kind = 8 ) ti2
  real    ( kind = 8 ) ti3
  real    ( kind = 8 ) ti4
  real    ( kind = 8 ) tr1
  real    ( kind = 8 ) tr2
  real    ( kind = 8 ) tr3
  real    ( kind = 8 ) tr4
  real    ( kind = 8 ) wa1(ido)
  real    ( kind = 8 ) wa2(ido)
  real    ( kind = 8 ) wa3(ido)

  hsqt2 = sqrt ( 2.0D+00 ) / 2.0D+00

  do k = 1, l1
    tr1 = cc(1,k,2) + cc(1,k,4)
    tr2 = cc(1,k,1) + cc(1,k,3)
    ch(1,1,k) = tr1 + tr2
    ch(ido,4,k) = tr2 - tr1
    ch(ido,2,k) = cc(1,k,1) - cc(1,k,3)
    ch(1,3,k) = cc(1,k,4) - cc(1,k,2)
  end do

  if ( ido < 2 ) then
    return
  end if

  if ( 2 < ido ) then

    do k = 1, l1
      do i = 3, ido, 2

        ic = ido + 2 - i

        cr2 = wa1(i-2) * cc(i-1,k,2) + wa1(i-1) * cc(i,k,2)
        ci2 = wa1(i-2) * cc(i,k,2)   - wa1(i-1) * cc(i-1,k,2)
        cr3 = wa2(i-2) * cc(i-1,k,3) + wa2(i-1) * cc(i,k,3)
        ci3 = wa2(i-2) * cc(i,k,3)   - wa2(i-1) * cc(i-1,k,3)
        cr4 = wa3(i-2) * cc(i-1,k,4) + wa3(i-1) * cc(i,k,4)
        ci4 = wa3(i-2) * cc(i,k,4)   - wa3(i-1) * cc(i-1,k,4)

        tr1 = cr2 + cr4
        tr4 = cr4 - cr2
        ti1 = ci2 + ci4
        ti4 = ci2 - ci4
        ti2 = cc(i,k,1) + ci3
        ti3 = cc(i,k,1) - ci3
        tr2 = cc(i-1,k,1) + cr3
        tr3 = cc(i-1,k,1) - cr3

        ch(i-1,1,k)  = tr1 + tr2
        ch(ic-1,4,k) = tr2 - tr1
        ch(i,1,k)    = ti1 + ti2
        ch(ic,4,k)   = ti1 - ti2
        ch(i-1,3,k)  = ti4 + tr3
        ch(ic-1,2,k) = tr3 - ti4
        ch(i,3,k)    = tr4 + ti3
        ch(ic,2,k)   = tr4 - ti3

      end do
    end do

    if ( mod ( ido, 2 ) == 1 ) then
      return
    end if

  end if

  do k = 1, l1

    ti1 = -hsqt2 * ( cc(ido,k,2) + cc(ido,k,4) )
    tr1 =  hsqt2 * ( cc(ido,k,2) - cc(ido,k,4) )

    ch(ido,1,k) = tr1 + cc(ido,k,1)
    ch(ido,3,k) = cc(ido,k,1) - tr1

    ch(1,2,k) = ti1 - cc(ido,k,3)
    ch(1,4,k) = ti1 + cc(ido,k,3)

  end do

  return
end
subroutine dadf5 ( ido, l1, cc, ch, wa1, wa2, wa3, wa4 )

!*****************************************************************************80
!
!! DADF5 is a lower level routine used by DFFTF1.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    07 January 2002
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) ido
  integer ( kind = 4 ) l1

  real    ( kind = 8 ) cc(ido,l1,5)
  real    ( kind = 8 ) ch(ido,5,l1)
  real    ( kind = 8 ) ci2
  real    ( kind = 8 ) ci3
  real    ( kind = 8 ) ci4
  real    ( kind = 8 ) ci5
  real    ( kind = 8 ) cr2
  real    ( kind = 8 ) cr3
  real    ( kind = 8 ) cr4
  real    ( kind = 8 ) cr5
  real    ( kind = 8 ) di2
  real    ( kind = 8 ) di3
  real    ( kind = 8 ) di4
  real    ( kind = 8 ) di5
  real    ( kind = 8 ) dr2
  real    ( kind = 8 ) dr3
  real    ( kind = 8 ) dr4
  real    ( kind = 8 ) dr5
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ic
  integer ( kind = 4 ) k
  real    ( kind = 8 ), parameter :: ti11 =  0.951056516295154D+00
  real    ( kind = 8 ), parameter :: ti12 =  0.587785252292473D+00
  real    ( kind = 8 ) ti2
  real    ( kind = 8 ) ti3
  real    ( kind = 8 ) ti4
  real    ( kind = 8 ) ti5
  real    ( kind = 8 ), parameter :: tr11 =  0.309016994374947D+00
  real    ( kind = 8 ), parameter :: tr12 = -0.809016994374947D+00
  real    ( kind = 8 ) tr2
  real    ( kind = 8 ) tr3
  real    ( kind = 8 ) tr4
  real    ( kind = 8 ) tr5
  real    ( kind = 8 ) wa1(ido)
  real    ( kind = 8 ) wa2(ido)
  real    ( kind = 8 ) wa3(ido)
  real    ( kind = 8 ) wa4(ido)

  do k = 1, l1

    cr2 = cc(1,k,5) + cc(1,k,2)
    ci5 = cc(1,k,5) - cc(1,k,2)
    cr3 = cc(1,k,4) + cc(1,k,3)
    ci4 = cc(1,k,4) - cc(1,k,3)

    ch(1,1,k)   = cc(1,k,1) + cr2 + cr3
    ch(ido,2,k) = cc(1,k,1) + tr11 * cr2 + tr12 * cr3
    ch(1,3,k)   = ti11 * ci5 + ti12 * ci4
    ch(ido,4,k) = cc(1,k,1) + tr12 * cr2 + tr11 * cr3
    ch(1,5,k)   = ti12 * ci5 - ti11 * ci4

  end do

  if ( ido == 1 ) then
    return
  end if

  do k = 1, l1
    do i = 3, ido, 2

      ic = ido + 2 - i

      dr2 = wa1(i-2) * cc(i-1,k,2) + wa1(i-1) * cc(i,k,2)
      di2 = wa1(i-2) * cc(i,k,2)   - wa1(i-1) * cc(i-1,k,2)
      dr3 = wa2(i-2) * cc(i-1,k,3) + wa2(i-1) * cc(i,k,3)
      di3 = wa2(i-2) * cc(i,k,3)   - wa2(i-1) * cc(i-1,k,3)
      dr4 = wa3(i-2) * cc(i-1,k,4) + wa3(i-1) * cc(i,k,4)
      di4 = wa3(i-2) * cc(i,k,4)   - wa3(i-1) * cc(i-1,k,4)
      dr5 = wa4(i-2) * cc(i-1,k,5) + wa4(i-1) * cc(i,k,5)
      di5 = wa4(i-2) * cc(i,k,5)   - wa4(i-1) * cc(i-1,k,5)

      cr2 = dr2 + dr5
      ci5 = dr5 - dr2
      cr5 = di2 - di5
      ci2 = di2 + di5
      cr3 = dr3 + dr4
      ci4 = dr4 - dr3
      cr4 = di3 - di4
      ci3 = di3 + di4

      ch(i-1,1,k) = cc(i-1,k,1) + cr2 + cr3
      ch(i,1,k)   = cc(i,k,1)   + ci2 + ci3

      tr2 = cc(i-1,k,1) + tr11 * cr2 + tr12 * cr3
      ti2 = cc(i,k,1)   + tr11 * ci2 + tr12 * ci3
      tr3 = cc(i-1,k,1) + tr12 * cr2 + tr11 * cr3
      ti3 = cc(i,k,1)   + tr12 * ci2 + tr11 * ci3

      tr5 = ti11 * cr5 + ti12 * cr4
      ti5 = ti11 * ci5 + ti12 * ci4
      tr4 = ti12 * cr5 - ti11 * cr4
      ti4 = ti12 * ci5 - ti11 * ci4

      ch(i-1,3,k)  = tr2 + tr5
      ch(ic-1,2,k) = tr2 - tr5
      ch(i,3,k)    = ti2 + ti5
      ch(ic,2,k)   = ti5 - ti2
      ch(i-1,5,k)  = tr3 + tr4
      ch(ic-1,4,k) = tr3 - tr4
      ch(i,5,k)    = ti3 + ti4
      ch(ic,4,k)   = ti4 - ti3

    end do
  end do

  return
end
subroutine dadfg ( ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa )

!*****************************************************************************80
!
!! DADFG is a lower level routine used by DFFTF1.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    07 January 2002
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) idl1
  integer ( kind = 4 ) ido
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) l1

  real    ( kind = 8 ) ai1
  real    ( kind = 8 ) ai2
  real    ( kind = 8 ) ar1
  real    ( kind = 8 ) ar1h
  real    ( kind = 8 ) ar2
  real    ( kind = 8 ) ar2h
  real    ( kind = 8 ) arg
  real    ( kind = 8 ) c1(ido,l1,ip)
  real    ( kind = 8 ) c2(idl1,ip)
  real    ( kind = 8 ) cc(ido,ip,l1)
  real    ( kind = 8 ) ch(ido,l1,ip)
  real    ( kind = 8 ) ch2(idl1,ip)
  real    ( kind = 8 ) dc2
  real    ( kind = 8 ) dcp
  real    ( kind = 8 ) ds2
  real    ( kind = 8 ) dsp
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ic
  integer ( kind = 4 ) idij
  integer ( kind = 4 ) ik
  integer ( kind = 4 ) ipph
  integer ( kind = 4 ) is
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) jc
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) lc
  integer ( kind = 4 ) nbd
  real    ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real    ( kind = 8 ) wa(*)

  arg = 2.0D+00 * pi / real ( ip, kind = 8 )
  dcp = cos ( arg )
  dsp = sin ( arg )
  ipph = ( ip + 1 ) / 2
  nbd = ( ido - 1 ) / 2

  if ( ido == 1 ) then

    c2(1:idl1,1) = ch2(1:idl1,1)

  else

    ch2(1:idl1,1) = c2(1:idl1,1)
    ch(1,1:l1,2:ip) = c1(1,1:l1,2:ip)

    if ( nbd <= l1 ) then

      is = -ido
      do j = 2, ip
        is = is + ido
        idij = is
        do i = 3, ido, 2
          idij = idij + 2
          do k = 1, l1
            ch(i-1,k,j) = wa(idij-1) * c1(i-1,k,j) + wa(idij) * c1(i,k,j)
            ch(i,k,j)   = wa(idij-1) * c1(i,k,j)   - wa(idij) * c1(i-1,k,j)
          end do
        end do
      end do

    else

      is = -ido

      do j = 2, ip
        is = is + ido
        do k = 1, l1
          idij = is
          do i = 3, ido, 2
            idij = idij + 2
            ch(i-1,k,j) = wa(idij-1) * c1(i-1,k,j) + wa(idij) * c1(i,k,j)
            ch(i,k,j)   = wa(idij-1) * c1(i,k,j)   - wa(idij) * c1(i-1,k,j)
          end do
        end do
      end do

    end if

    if ( l1 <= nbd ) then

      do j = 2, ipph
        jc = ip + 2 - j
        do k = 1, l1
          do i = 3, ido, 2
            c1(i-1,k,j)  = ch(i-1,k,j)  + ch(i-1,k,jc)
            c1(i-1,k,jc) = ch(i,k,j)    - ch(i,k,jc)
            c1(i,k,j)    = ch(i,k,j)    + ch(i,k,jc)
            c1(i,k,jc)   = ch(i-1,k,jc) - ch(i-1,k,j)
          end do
        end do
      end do

    else

      do j = 2, ipph
        jc = ip + 2 - j
        do i = 3, ido, 2
          c1(i-1,1:l1,j)  = ch(i-1,1:l1,j)  + ch(i-1,1:l1,jc)
          c1(i-1,1:l1,jc) = ch(i,1:l1,j)    - ch(i,1:l1,jc)
          c1(i,1:l1,j)    = ch(i,1:l1,j)    + ch(i,1:l1,jc)
          c1(i,1:l1,jc)   = ch(i-1,1:l1,jc) - ch(i-1,1:l1,j)
        end do
      end do

    end if

  end if

  do j = 2, ipph
    jc = ip + 2 - j
    c1(1,1:l1,j)  = ch(1,1:l1,j)  + ch(1,1:l1,jc)
    c1(1,1:l1,jc) = ch(1,1:l1,jc) - ch(1,1:l1,j)
  end do

  ar1 = 1.0E+00
  ai1 = 0.0E+00

  do l = 2, ipph

    lc = ip + 2 - l
    ar1h = dcp * ar1 - dsp * ai1
    ai1 =  dcp * ai1 + dsp * ar1
    ar1 = ar1h

    do ik = 1, idl1
      ch2(ik,l) = c2(ik,1) + ar1 * c2(ik,2)
      ch2(ik,lc) =           ai1 * c2(ik,ip)
    end do

    dc2 = ar1
    ds2 = ai1
    ar2 = ar1
    ai2 = ai1

    do j = 3, ipph

      jc = ip + 2 - j
      ar2h = dc2 * ar2 - ds2 * ai2
      ai2 =  dc2 * ai2 + ds2 * ar2
      ar2 = ar2h

      do ik = 1, idl1
        ch2(ik,l) =  ch2(ik,l)  + ar2 * c2(ik,j)
        ch2(ik,lc) = ch2(ik,lc) + ai2 * c2(ik,jc)
      end do

    end do

  end do

  do j = 2, ipph
    ch2(1:idl1,1) = ch2(1:idl1,1) + c2(1:idl1,j)
  end do

  cc(1:ido,1,1:l1) = ch(1:ido,1:l1,1)

  do j = 2, ipph
    jc = ip + 2 - j
    j2 = j + j
    cc(ido,j2-2,1:l1) = ch(1,1:l1,j)
    cc(1,j2-1,1:l1)   = ch(1,1:l1,jc)
  end do

  if ( ido == 1 ) then
    return
  end if

  if ( l1 <= nbd ) then

    do j = 2, ipph
      jc = ip + 2 - j
      j2 = j + j
      do k = 1, l1
        do i = 3, ido, 2
          ic = ido + 2 - i
          cc(i-1,j2-1,k)  = ch(i-1,k,j) + ch(i-1,k,jc)
          cc(ic-1,j2-2,k) = ch(i-1,k,j) - ch(i-1,k,jc)
          cc(i,j2-1,k)    = ch(i,k,j)   + ch(i,k,jc)
          cc(ic,j2-2,k)   = ch(i,k,jc)  - ch(i,k,j)
        end do
      end do
    end do

  else

    do j = 2, ipph
      jc = ip + 2 - j
      j2 = j + j
      do i = 3, ido, 2
        ic = ido + 2 - i
        cc(i-1,j2-1,1:l1)  = ch(i-1,1:l1,j) + ch(i-1,1:l1,jc)
        cc(ic-1,j2-2,1:l1) = ch(i-1,1:l1,j) - ch(i-1,1:l1,jc)
        cc(i,j2-1,1:l1)    = ch(i,1:l1,j)   + ch(i,1:l1,jc)
        cc(ic,j2-2,1:l1)   = ch(i,1:l1,jc)  - ch(i,1:l1,j)
      end do
    end do

  end if

  return
end
subroutine dcost ( n, x, wsave )

!*****************************************************************************80
!
!! DCOST computes the discrete Fourier cosine transform of an even sequence.
!
!  Discussion:
!
!    This routine is the unnormalized inverse of itself.  Two successive
!    calls will multiply the input sequence X by 2*(N-1).
!
!    The array WSAVE must be initialized by calling DCOSTI.
!
!    The transform is defined by:
!
!      X_out(I) = X_in(1) + (-1) **(I-1) * X_in(N) + sum ( 2 <= K <= N-1 )
!
!        2 * X_in(K) * cos ( ( K - 1 ) * ( I - 1 ) * PI / ( N - 1 ) )
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    07 January 2002
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    Paul Swarztrauber,
!    Vectorizing the FFT's,
!    in Parallel Computations,
!    G. Rodrigue, editor,
!    Academic Press, 1982, pages 51-83.
!
!    Bill Buzbee,
!    The SLATEC Common Math Library,
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the sequence to be transformed.  The
!    method is more efficient when N-1 is the product of small primes.
!
!    Input/output, real ( kind = 8 ) X(N).
!    On input, the sequence to be transformed.
!    On output, the transformed sequence.
!
!    Input, real ( kind = 8 ) WSAVE(3*N+15).
!    The WSAVE array must be initialized by calling DCOSTI.  A different
!    array must be used for each different value of N.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 8 ) c1
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kc
  integer ( kind = 4 ) ns2
  real    ( kind = 8 ) t1
  real    ( kind = 8 ) t2
  real    ( kind = 8 ) tx2
  real    ( kind = 8 ) wsave(3*n+15)
  real    ( kind = 8 ) x(n)
  real    ( kind = 8 ) x1h
  real    ( kind = 8 ) x1p3
  real    ( kind = 8 ) xi
  real    ( kind = 8 ) xim2

  ns2 = n / 2

  if ( n <= 1 ) then
    return
  end if

  if ( n == 2 ) then
    x1h = x(1) + x(2)
    x(2) = x(1) - x(2)
    x(1) = x1h
    return
  end if

  if ( n == 3 ) then
    x1p3 = x(1) + x(3)
    tx2 = x(2) + x(2)
    x(2) = x(1) - x(3)
    x(1) = x1p3 + tx2
    x(3) = x1p3 - tx2
    return
  end if

  c1 = x(1) - x(n)
  x(1) = x(1) + x(n)

  do k = 2, ns2
    kc = n + 1 - k
    t1 = x(k) + x(kc)
    t2 = x(k) - x(kc)
    c1 = c1 + wsave(kc) * t2
    t2 = wsave(k) * t2
    x(k) = t1 - t2
    x(kc) = t1 + t2
  end do

  if ( mod ( n, 2 ) /= 0 ) then
    x(ns2+1) = x(ns2+1) + x(ns2+1)
  end if

  call dfftf ( n-1, x, wsave(n+1) )

  xim2 = x(2)
  x(2) = c1

  do i = 4, n, 2
    xi = x(i)
    x(i) = x(i-2) - x(i-1)
    x(i-1) = xim2
    xim2 = xi
  end do

  if ( mod ( n, 2 ) /= 0 ) then
    x(n) = xim2
  end if

  return
end
subroutine dcosti ( n, wsave )

!*****************************************************************************80
!
!! DCOSTI initializes WSAVE, used in DCOST.
!
!  Discussion:
!
!    The prime factorization of N together with a tabulation of the
!    trigonometric functions are computed and stored in WSAVE.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    07 January 2002
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    Paul Swarztrauber,
!    Vectorizing the FFT's,
!    in Parallel Computations,
!    G. Rodrigue, editor,
!    Academic Press, 1982, pages 51-83.
!
!    Bill Buzbee,
!    The SLATEC Common Math Library,
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the sequence to be transformed.  The
!    method is more efficient when N-1 is the product of small primes.
!
!    Output, real ( kind = 8 ) WSAVE(3*N+15), contains data, depending on N, and
!    required by the DCOST algorithm.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 8 ) dt
  integer ( kind = 4 ) k
  real    ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real    ( kind = 8 ) wsave(3*n+15)

  if ( n <= 3 ) then
    return
  end if

  dt = pi / real ( n - 1, kind = 8 )

  do k = 2, ( n / 2 )
    wsave(k)     = 2.0D+00 * sin ( real ( k - 1, kind = 8 ) * dt )
    wsave(n+1-k) = 2.0D+00 * cos ( real ( k - 1, kind = 8 ) * dt )
  end do

  call dffti ( n-1, wsave(n+1) )

  return
end
subroutine dfftf ( n, r, wsave )

!*****************************************************************************80
!
!! DFFTF computes the Fourier coefficients of a real periodic sequence.
!
!  Discussion:
!
!    This process is sometimes called Fourier analysis.
!
!    The transform is unnormalized.  A call to DFFTF followed by a call
!    to DFFTB will multiply the input sequence by N.
!
!    The transform is defined by:
!
!      R_out(1) = sum ( 1 <= I <= N ) R_in(I)
!
!    Letting L = (N+1)/2, then for K = 2,...,L
!
!      R_out(2*K-2) = sum ( 1 <= I <= N )
!
!        R_in(I) * cos ( ( K - 1 ) * ( I - 1 ) * 2 * PI / N )
!
!      R_out(2*K-1) = sum ( 1 <= I <= N )
!
!        -R_in(I) * sin ( ( K - 1 ) * ( I - 1 ) * 2 * PI / N )
!
!    And, if N is even, then:
!
!      R_out(N) = sum ( 1 <= I <= N ) (-1)**(I-1) * R_in(I)
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    07 January 2002
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    Paul Swarztrauber,
!    Vectorizing the FFT's,
!    in Parallel Computations,
!    G. Rodrigue, editor,
!    Academic Press, 1982, pages 51-83.
!
!    Bill Buzbee,
!    The SLATEC Common Math Library,
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the array to be transformed.  The
!    method is more efficient when N is the product of small primes.
!
!    Input/output, real ( kind = 8 ) R(N).
!    On input, the sequence to be transformed.
!    On output, the transformed sequence.
!
!    Input, real ( kind = 8 ) WSAVE(2*N+15), a work array.  The WSAVE array
!    must be initialized by calling DFFTI.  A different WSAVE array must be
!    used for each different value of N.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 8 ) r(n)
  real    ( kind = 8 ) wsave(2*n+15)

  if ( n <= 1 ) then
    return
  end if

  call dfftf1 ( n, r, wsave(1), wsave(n+1), wsave(2*n+1) )

  return
end
subroutine dfftf1 ( n, c, ch, wa, ifac )

!*****************************************************************************80
!
!! DFFTF1 is a lower level routine used by DFFTF and DSINT.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    07 January 2002
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the array to be transformed.
!
!    Input/output, real ( kind = 8 ) C(N).
!    On input, the sequence to be transformed.
!    On output, the transformed sequence.
!
!    Input, real ( kind = 8 ) CH(N).
!
!    Input, real ( kind = 8 ) WA(N).
!
!    Input, integer ( kind = 4 ) IFAC(15).
!    IFAC(1) = N, the number that was factored.
!    IFAC(2) = NF, the number of factors.
!    IFAC(3:2+NF), the factors.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 8 ) c(n)
  real    ( kind = 8 ) ch(n)
  integer ( kind = 4 ) idl1
  integer ( kind = 4 ) ido
  integer ( kind = 4 ) ifac(15)
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) iw
  integer ( kind = 4 ) ix2
  integer ( kind = 4 ) ix3
  integer ( kind = 4 ) ix4
  integer ( kind = 4 ) k1
  integer ( kind = 4 ) kh
  integer ( kind = 4 ) l1
  integer ( kind = 4 ) l2
  integer ( kind = 4 ) na
  integer ( kind = 4 ) nf
  real    ( kind = 8 ) wa(n)

  nf = ifac(2)
  na = 1
  l2 = n
  iw = n

  do k1 = 1, nf

    kh = nf - k1
    ip = ifac(kh+3)
    l1 = l2 / ip
    ido = n / l2
    idl1 = ido * l1
    iw = iw - ( ip - 1 ) * ido
    na = 1 - na

    if ( ip == 4 ) then

      ix2 = iw + ido
      ix3 = ix2 + ido

      if ( na == 0 ) then
        call dadf4 ( ido, l1, c, ch, wa(iw), wa(ix2), wa(ix3) )
      else
        call dadf4 ( ido, l1, ch, c, wa(iw), wa(ix2), wa(ix3) )
      end if

    else if ( ip == 2 ) then

      if ( na == 0 ) then
        call dadf2 ( ido, l1, c, ch, wa(iw) )
      else
        call dadf2 ( ido, l1, ch, c, wa(iw) )
      end if

    else if ( ip == 3 ) then

      ix2 = iw + ido

      if ( na == 0 ) then
        call dadf3 ( ido, l1, c, ch, wa(iw), wa(ix2) )
      else
        call dadf3 ( ido, l1, ch, c, wa(iw), wa(ix2) )
      end if

    else if ( ip == 5 ) then

      ix2 = iw + ido
      ix3 = ix2 + ido
      ix4 = ix3 + ido

      if ( na == 0 ) then
        call dadf5 ( ido, l1, c, ch, wa(iw), wa(ix2), wa(ix3), wa(ix4) )
      else
        call dadf5 ( ido, l1, ch, c, wa(iw), wa(ix2), wa(ix3), wa(ix4) )
      end if

    else

      if ( ido == 1 ) then
        na = 1 - na
      end if

      if ( na == 0 ) then
        call dadfg ( ido, ip, l1, idl1, c, c, c, ch, ch, wa(iw) )
        na = 1
      else
        call dadfg ( ido, ip, l1, idl1, ch, ch, ch, c, c, wa(iw) )
        na = 0
      end if

    end if

    l2 = l1

  end do

  if ( na /= 1 ) then
    c(1:n) = ch(1:n)
  end if

  return
end
subroutine dffti ( n, wsave )

!*****************************************************************************80
!
!! DFFTI initializes WSAVE, used in DFFTF and DFFTB.
!
!  Discussion:
!
!    The prime factorization of N together with a tabulation of the
!    trigonometric functions are computed and stored in WSAVE.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    07 January 2002
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    Paul Swarztrauber,
!    Vectorizing the FFT's,
!    in Parallel Computations,
!    G. Rodrigue, editor,
!    Academic Press, 1982, pages 51-83.
!
!    Bill Buzbee,
!    The SLATEC Common Math Library,
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the sequence to be transformed.
!
!    Output, real ( kind = 8 ) WSAVE(2*N+15), contains data, dependent
!    on the value of N, which is necessary for the DFFTF and DFFTB routines.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 8 ) wsave(2*n+15)

  if ( n <= 1 ) then
    return
  end if

  call dffti1 ( n, wsave(n+1), wsave(2*n+1) )

  return
end
subroutine dffti1 ( n, wa, ifac )

!*****************************************************************************80
!
!! DFFTI1 is a lower level routine used by DFFTI.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    07 January 2002
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the sequence to be transformed.
!
!    Input, real ( kind = 8 ) WA(N).
!
!    Input, integer ( kind = 4 ) IFAC(15).
!    IFAC(1) = N, the number that was factored.
!    IFAC(2) = NF, the number of factors.
!    IFAC(3:2+NF), the factors.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 8 ) arg
  real    ( kind = 8 ) argh
  real    ( kind = 8 ) argld
  real    ( kind = 8 ) fi
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ido
  integer ( kind = 4 ) ifac(15)
  integer ( kind = 4 ) ii
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) is
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k1
  integer ( kind = 4 ) l1
  integer ( kind = 4 ) l2
  integer ( kind = 4 ) ld
  integer ( kind = 4 ) nf
  real    ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real    ( kind = 8 ) wa(n)

  call i4_factor ( n, ifac )

  nf = ifac(2)

  argh = 2.0D+00 * pi / real ( n, kind = 8 )
  is = 0
  l1 = 1

  do k1 = 1, nf-1

    ip = ifac(k1+2)
    ld = 0
    l2 = l1 * ip
    ido = n / l2

    do j = 1, ip-1

      ld = ld + l1
      i = is
      argld = real ( ld, kind = 8 ) * argh
      fi = 0.0D+00

      do ii = 3, ido, 2
        i = i + 2
        fi = fi + 1.0D+00
        arg = fi * argld
        wa(i-1) = cos ( arg )
        wa(i) = sin ( arg )
      end do

      is = is + ido

    end do

    l1 = l2

  end do

  return
end
subroutine dsint ( n, x, wsave )

!*****************************************************************************80
!
!! DSINT computes the discrete Fourier sine transform of an odd sequence.
!
!  Discussion:
!
!    This routine is the unnormalized inverse of itself since two successive
!    calls will multiply the input sequence X by 2*(N+1).
!
!    The array WSAVE must be initialized by calling DSINTI.
!
!    The transform is defined by:
!
!      X_out(I) = sum ( 1 <= K <= N )
!        2 * X_in(K) * sin ( K * I * PI / ( N + 1 ) )
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    07 January 2002
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    Paul Swarztrauber,
!    Vectorizing the FFT's,
!    in Parallel Computations,
!    G. Rodrigue, editor,
!    Academic Press, 1982, pages 51-83.
!
!    Bill Buzbee,
!    The SLATEC Common Math Library,
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the sequence to be transformed.
!    The method is most efficient when N+1 is the product of small primes.
!
!    Input/output, real ( kind = 4 ) X(N).
!    On input, the sequence to be transformed.
!    On output, the transformed sequence.
!
!    Input, real ( kind = 8 ) WSAVE((5*N+30)/2), a work array.  The WSAVE
!    array must be initialized by calling DSINTI.  A different WSAVE array
!    must be used for each different value of N.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) iw1
  integer ( kind = 4 ) iw2
  integer ( kind = 4 ) iw3
  real    ( kind = 8 ) wsave((5*n+30)/2)
  real    ( kind = 8 ) x(n)

  iw1 = n / 2 + 1
  iw2 = iw1 + n + 1
  iw3 = iw2 + n + 1

  call dsint1 ( n, x, wsave(1), wsave(iw1), wsave(iw2), wsave(iw3) )

  return
end
subroutine dsint1 ( n, war, was, xh, x, ifac )

!*****************************************************************************80
!
!! DSINT1 is a lower level routine used by DSINT.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    07 January 2002
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the sequence to be transformed.
!
!    Input/output, real ( kind = 8 ) WAR(N).
!    On input, the sequence to be transformed.
!    On output, the transformed sequence.
!
!    Input, real ( kind = 8 ) WAS(N/2).
!
!    Input, real ( kind = 8 ) XH(N).
!
!    Input, real ( kind = 8 ) X(N+1), ?.
!
!    Input, integer ( kind = 4 ) IFAC(15).
!    IFAC(1) = N, the number that was factored.
!    IFAC(2) = NF, the number of factors.
!    IFAC(3:2+NF), the factors.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ifac(15)
  integer ( kind = 4 ) k
  integer ( kind = 4 ) ns2
  real    ( kind = 8 ) sqrt3
  real    ( kind = 8 ) t1
  real    ( kind = 8 ) t2
  real    ( kind = 8 ) war(n)
  real    ( kind = 8 ) was(n/2)
  real    ( kind = 8 ) x(n+1)
  real    ( kind = 8 ) xh(n)
  real    ( kind = 8 ) xhold

  sqrt3 = sqrt ( 3.0D+00 )

  xh(1:n) = war(1:n)
  war(1:n) = x(1:n)

  if ( n <= 1 ) then
    xh(1) = 2.0D+00 * xh(1)
    return
  end if

  if ( n == 2 ) then
    xhold = sqrt3 * ( xh(1) + xh(2) )
    xh(2) = sqrt3 * ( xh(1) - xh(2) )
    xh(1) = xhold
    return
  end if

  ns2 = n / 2
  x(1) = 0.0D+00

  do k = 1, n/2
    t1 = xh(k) - xh(n+1-k)
    t2 = was(k) * ( xh(k) + xh(n+1-k) )
    x(k+1) = t1 + t2
    x(n+2-k) = t2 - t1
  end do

  if ( mod ( n, 2 ) /= 0 ) then
    x(n/2+2) = 4.0D+00 * xh(n/2+1)
  end if

  call dfftf1 ( n+1, x, xh, war, ifac )

  xh(1) = 0.5D+00 * x(1)
  do i = 3, n, 2
    xh(i-1) = -x(i)
    xh(i) = xh(i-2) + x(i-1)
  end do

  if ( mod ( n, 2 ) == 0 ) then
    xh(n) = -x(n+1)
  end if

  x(1:n) = war(1:n)
  war(1:n) = xh(1:n)

  return
end
subroutine dsinti ( n, wsave )

!*****************************************************************************80
!
!! DSINTI initializes WSAVE, used in DSINT.
!
!  Discussion:
!
!    The prime factorization of N together with a tabulation of the
!    trigonometric functions are computed and stored in WSAVE.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    07 January 2002
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    Paul Swarztrauber,
!    Vectorizing the FFT's,
!    in Parallel Computations,
!    G. Rodrigue, editor,
!    Academic Press, 1982, pages 51-83.
!
!    Bill Buzbee,
!    The SLATEC Common Math Library,
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the sequence to be transformed.
!    The method is most efficient when N+1 is a product of small primes.
!
!    Output, real ( kind = 8 ) WSAVE((5*N+30)/2), contains data, dependent
!    on the value of N, which is necessary for the DSINT routine.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 8 ) dt
  integer ( kind = 4 ) k
  real    ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real    ( kind = 8 ) wsave((5*n+30)/2)

  if ( n <= 1 ) then
    return
  end if

  dt = pi / real ( n + 1, kind = 8 )

  do k = 1, n/2
    wsave(k) = 2.0D+00 * sin ( real ( k, kind = 8 ) * dt )
  end do

  call dffti ( n+1, wsave((n/2)+1) )

  return
end
subroutine ezfftb ( n, r, azero, a, b, wsave )

!*****************************************************************************80
!
!! EZFFTB computes a real periodic sequence from its Fourier coefficients.
!
!  Discussion:
!
!    This process is sometimes called Fourier synthesis.
!
!    EZFFTB is a simplified but slower version of RFFTB.
!
!    The transform is defined by:
!
!      R(I) = AZERO + sum ( 1 <= K <= N/2 )
!
!          A(K) * cos ( K * ( I - 1 ) * 2 * PI / N )
!        + B(K) * sin ( K * ( I - 1 ) * 2 * PI / N )
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    Paul Swarztrauber,
!    Vectorizing the FFT's,
!    in Parallel Computations,
!    G. Rodrigue, editor,
!    Academic Press, 1982, pages 51-83.
!
!    Bill Buzbee,
!    The SLATEC Common Math Library,
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the output array.  The
!    method is more efficient when N is the product of small primes.
!
!    Output, real ( kind = 4 ) R(N), the reconstructed data sequence.
!
!    Input, real ( kind = 4 ) AZERO, the constant Fourier coefficient.
!
!    Input, real ( kind = 4 ) A(N/2), B(N/2), the Fourier coefficients.
!
!    Input, real ( kind = 4 ) WSAVE(3*N+15), a work array.  The WSAVE array must be
!    initialized by calling EZFFFTI.  A different WSAVE array must be used
!    for each different value of N.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 4 ) a(n/2)
  real    ( kind = 4 ) azero
  real    ( kind = 4 ) b(n/2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ns2
  real    ( kind = 4 ) r(n)
  real    ( kind = 4 ) wsave(3*n+15)

  if ( n < 2 ) then

    r(1) = azero

  else if ( n == 2 ) then

    r(1) = azero + a(1)
    r(2) = azero - a(1)

  else

    ns2 = ( n - 1 ) / 2

    do i = 1, ns2
      r(2*i) =    0.5E+00 * a(i)
      r(2*i+1) = -0.5E+00 * b(i)
    end do

    r(1) = azero

    if ( mod ( n, 2 ) == 0 ) then
      r(n) = a(ns2+1)
    end if

    call rfftb ( n, r, wsave(n+1) )

  end if

  return
end
subroutine ezfftf ( n, r, azero, a, b, wsave )

!*****************************************************************************80
!
!! EZFFTF computes the Fourier coefficients of a real periodic sequence.
!
!  Discussion:
!
!    This process is sometimes called Fourier analysis.
!
!    EZFFTF is a simplified but slower version of RFFTF.
!
!    The transform is defined by:
!
!      AZERO = sum ( 1 <= I <= N ) R(I) / N,
!
!    and, for K = 1 to (N-1)/2,
!
!      A(K) = sum ( 1 <= I <= N )
!        ( 2 / N ) * R(I) * cos ( K * ( I - 1 ) * 2 * PI / N )
!
!    and, if N is even, then
!
!      A(N/2) = sum ( 1 <= I <= N ) (-1) **(I-1) * R(I) / N
!
!    For K = 1 to (N-1)/2,
!
!      B(K) = sum ( 1 <= I <= N )
!        ( 2 / N ) * R(I) * sin ( K * ( I - 1 ) * 2 * PI / N )
!
!    and, if N is even, then
!
!      B(N/2) = 0.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    Paul Swarztrauber,
!    Vectorizing the FFT's,
!    in Parallel Computations,
!    G. Rodrigue, editor,
!    Academic Press, 1982, pages 51-83.
!
!    Bill Buzbee,
!    The SLATEC Common Math Library,
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the array to be transformed.  The
!    method is more efficient when N is the product of small primes.
!
!    Input, real ( kind = 4 ) R(N), the sequence to be transformed.
!
!    Input, real ( kind = 4 ) WSAVE(3*N+15), a work array.  The WSAVE array must be
!    initialized by calling EZFFTI.  A different WSAVE array must be used
!    for each different value of N.
!
!    Output, real ( kind = 4 ) AZERO, the constant Fourier coefficient.
!
!    Output, real ( kind = 4 ) A(N/2), B(N/2), the Fourier coefficients.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 4 ) a(n/2)
  real    ( kind = 4 ) azero
  real    ( kind = 4 ) b(n/2)
  real    ( kind = 4 ) cf
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ns2
  real    ( kind = 4 ) r(n)
  real    ( kind = 4 ) wsave(3*n+15)

  if ( n < 2 ) then

    azero = r(1)

  else if ( n == 2 ) then

    azero = 0.5E+00 * ( r(1) + r(2) )
    a(1) = 0.5E+00 * ( r(1) - r(2) )

  else

    wsave(1:n) = r(1:n)

    call rfftf ( n, wsave(1), wsave(n+1) )

    cf = 2.0E+00 / real ( n, kind = 4 )
    azero = 0.5E+00 * cf * wsave(1)
    ns2 = ( n + 1 ) / 2

    do i = 1, ns2-1
      a(i) = cf * wsave(2*i)
      b(i) = -cf * wsave(2*i+1)
    end do

    if ( mod ( n, 2 ) /= 1 ) then
      a(ns2) = 0.5E+00 * cf * wsave(n)
      b(ns2) = 0.0E+00
    end if

  end if

  return
end
subroutine ezffti ( n, wsave )

!*****************************************************************************80
!
!! EZFFTI initializes WSAVE, used in EZFFTF and EZFFTB.
!
!  Discussion:
!
!    The prime factorization of N together with a tabulation of the
!    trigonometric functions are computed and stored in WSAVE.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    Paul Swarztrauber,
!    Vectorizing the FFT's,
!    in Parallel Computations,
!    G. Rodrigue, editor,
!    Academic Press, 1982, pages 51-83.
!
!    Bill Buzbee,
!    The SLATEC Common Math Library,
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the array to be transformed.  The
!    method is more efficient when N is the product of small primes.
!
!    Output, real ( kind = 4 ) WSAVE(3*N+15), contains data, dependent on the value
!    of N, which is necessary for the EZFFTF or EZFFTB routines.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 4 ) wsave(3*n+15)

  if ( n <= 1 ) then
    return
  end if

  call ezffti1 ( n, wsave(2*n+1), wsave(3*n+1) )

  return
end
subroutine ezffti1 ( n, wa, ifac )

!*****************************************************************************80
!
!! EZFFTI1 is a lower level routine used by EZFFTI.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the array to be transformed.
!
!    Output, real ( kind = 4 ) WA(N).
!
!    Input, integer ( kind = 4 ) IFAC(15).
!    IFAC(1) = N, the number that was factored.
!    IFAC(2) = NF, the number of factors.
!    IFAC(3:2+NF), the factors.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 4 ) arg1
  real    ( kind = 4 ) argh
  real    ( kind = 4 ) ch1
  real    ( kind = 4 ) ch1h
  real    ( kind = 4 ) dch1
  real    ( kind = 4 ) dsh1
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ido
  integer ( kind = 4 ) ifac(15)
  integer ( kind = 4 ) ii
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) is
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k1
  integer ( kind = 4 ) l1
  integer ( kind = 4 ) l2
  integer ( kind = 4 ) nf
  real    ( kind = 4 ), parameter :: pi = 3.1415926E+00
  real    ( kind = 4 ) sh1
  real    ( kind = 4 ) wa(n)

  call i4_factor ( n, ifac )

  nf = ifac(2)

  argh = 2.0E+00 * pi / real ( n, kind = 4 )
  is = 0
  l1 = 1

  do k1 = 1, nf-1

    ip = ifac(k1+2)
    l2 = l1 * ip
    ido = n / l2
    arg1 = real ( l1, kind = 4 ) * argh
    ch1 = 1.0E+00
    sh1 = 0.0E+00
    dch1 = cos ( arg1 )
    dsh1 = sin ( arg1 )

    do j = 1, ip-1

      ch1h = dch1 * ch1 - dsh1 * sh1
      sh1  = dch1 * sh1 + dsh1 * ch1
      ch1 = ch1h
      i = is + 2
      wa(i-1) = ch1
      wa(i) = sh1

      do ii = 5, ido, 2
        i = i + 2
        wa(i-1) = ch1 * wa(i-3) - sh1 * wa(i-2)
        wa(i)   = ch1 * wa(i-2) + sh1 * wa(i-3)
      end do

      is = is + ido

    end do

    l1 = l2

  end do

  return
end
subroutine i4_factor ( n, ifac )

!*****************************************************************************80
!
!! I4_FACTOR factors an integer.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    14 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number to be factored.
!
!    Output, integer ( kind = 4 ) IFAC(15).
!    IFAC(1) = N, the number that was factored.
!    IFAC(2) = NF, the number of factors.
!    IFAC(3:2+NF), the factors.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ifac(15)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nf
  integer ( kind = 4 ) nl
  integer ( kind = 4 ) nq
  integer ( kind = 4 ) nr
  integer ( kind = 4 ) ntry

  ifac(1) = n

  nf = 0
  nl = n

  if ( n == 0 ) then
    nf = 1
    ifac(2) = nf
    ifac(2+nf) = 0
    return
  end if

  if ( n < 1 ) then
    nf = nf + 1
    ifac(2+nf) = -1
    nl = - n
  end if

  if ( nl == 1 ) then
    nf = nf + 1
    ifac(2) = nf
    ifac(2+nf) = 1
    return
  end if

  j = 0

  do while ( 1 < nl )

    j = j + 1
!
!  Choose a trial divisor, NTRY.
!
    if ( j == 1 ) then
      ntry = 4
    else if ( j == 2 ) then
      ntry = 2
    else if ( j == 3 ) then
      ntry = 3
    else if ( j == 4 ) then
      ntry = 5
    else
      ntry = ntry + 2
    end if
!
!  Divide by the divisor as many times as possible.
!
    do

      nq = nl / ntry
      nr = nl - ntry * nq

      if ( nr /= 0 ) then
        exit
      end if

      nl = nq
      nf = nf + 1
!
!  Make sure factors of 2 appear in the front of the list.
!
      if ( ntry /= 2 ) then

        ifac(2+nf) = ntry

      else

        do i = nf, 2, -1
          ifac(i+2) = ifac(i+1)
        end do
        ifac(3) = 2

      end if

    end do

  end do

  ifac(2) = nf

  return
end
subroutine passb ( nac, ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa )

!*****************************************************************************80
!
!! PASSB is a lower level routine used by CFFTB1.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) idl1
  integer ( kind = 4 ) ido
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) l1

  real    ( kind = 4 ) c1(ido,l1,ip)
  real    ( kind = 4 ) c2(idl1,ip)
  real    ( kind = 4 ) cc(ido,ip,l1)
  real    ( kind = 4 ) ch(ido,l1,ip)
  real    ( kind = 4 ) ch2(idl1,ip)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) idij
  integer ( kind = 4 ) idj
  integer ( kind = 4 ) idl
  integer ( kind = 4 ) idlj
  integer ( kind = 4 ) idp
  integer ( kind = 4 ) ik
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) ipph
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jc
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) lc
  integer ( kind = 4 ) nac
  integer ( kind = 4 ) nt
  real    ( kind = 4 ) wa(*)
  real    ( kind = 4 ) wai
  real    ( kind = 4 ) war

  nt = ip * idl1
  ipph = ( ip + 1 ) / 2
  idp = ip * ido

  if ( l1 <= ido ) then

    do j = 2, ipph
      jc = ip + 2 - j
      do k = 1, l1
        ch(1:ido,k,j)  = cc(1:ido,j,k) + cc(1:ido,jc,k)
        ch(1:ido,k,jc) = cc(1:ido,j,k) - cc(1:ido,jc,k)
      end do
    end do

    ch(1:ido,1:l1,1) = cc(1:ido,1,1:l1)

  else

    do j = 2, ipph
      jc = ip + 2 - j
      do i = 1, ido
        ch(i,1:l1,j)  = cc(i,j,1:l1) + cc(i,jc,1:l1)
        ch(i,1:l1,jc) = cc(i,j,1:l1) - cc(i,jc,1:l1)
      end do
    end do

    ch(1:ido,1:l1,1) = cc(1:ido,1,1:l1)

  end if

  idl = 2 - ido
  inc = 0

  do l = 2, ipph

    lc = ip + 2 - l
    idl = idl + ido

    do ik = 1, idl1
      c2(ik,l) = ch2(ik,1) + wa(idl-1) * ch2(ik,2)
      c2(ik,lc) =            wa(idl)   * ch2(ik,ip)
    end do

    idlj = idl
    inc = inc + ido

    do j = 3, ipph

      jc = ip + 2 - j
      idlj = idlj + inc
      if ( idp < idlj ) then
        idlj = idlj - idp
      end if

      war = wa(idlj-1)
      wai = wa(idlj)

      do ik = 1, idl1
        c2(ik,l)  = c2(ik,l)  + war * ch2(ik,j)
        c2(ik,lc) = c2(ik,lc) + wai * ch2(ik,jc)
      end do

    end do

  end do

  do j = 2, ipph
    ch2(1:idl1,1) = ch2(1:idl1,1) + ch2(1:idl1,j)
  end do

  do j = 2, ipph
    jc = ip + 2 - j
    do ik = 2, idl1, 2
      ch2(ik-1,j)  = c2(ik-1,j) - c2(ik,jc)
      ch2(ik-1,jc) = c2(ik-1,j) + c2(ik,jc)
      ch2(ik,j)    = c2(ik,j)   + c2(ik-1,jc)
      ch2(ik,jc)   = c2(ik,j)   - c2(ik-1,jc)
    end do
  end do

  nac = 1

  if ( ido == 2 ) then
    return
  end if

  nac = 0
  c2(1:idl1,1) = ch2(1:idl1,1)
  c1(1:2,1:l1,2:ip) = ch(1:2,1:l1,2:ip)

  if ( ( ido / 2 ) <= l1 ) then

    idij = 0
    do j = 2, ip
      idij = idij + 2
      do i = 4, ido, 2
        idij = idij + 2
        c1(i-1,1:l1,j) = wa(idij-1) * ch(i-1,1:l1,j) - wa(idij) * ch(i,1:l1,j)
        c1(i,1:l1,j)   = wa(idij-1) * ch(i,1:l1,j)   + wa(idij) * ch(i-1,1:l1,j)
      end do
    end do

  else

    idj = 2 - ido

    do j = 2, ip
      idj = idj + ido
      do k = 1, l1
        idij = idj
        do i = 4, ido, 2
          idij = idij + 2
          c1(i-1,k,j) = wa(idij-1) * ch(i-1,k,j) - wa(idij) * ch(i,k,j)
          c1(i,k,j)   = wa(idij-1) * ch(i,k,j)   + wa(idij) * ch(i-1,k,j)
        end do
      end do
    end do

  end if

  return
end
subroutine passb2 ( ido, l1, cc, ch, wa1 )

!*****************************************************************************80
!
!! PASSB2 is a lower level routine used by CFFTB1.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) ido
  integer ( kind = 4 ) l1

  real    ( kind = 4 ) cc(ido,2,l1)
  real    ( kind = 4 ) ch(ido,l1,2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  real    ( kind = 4 ) ti2
  real    ( kind = 4 ) tr2
  real    ( kind = 4 ) wa1(ido)

  if ( ido <= 2 ) then

    ch(1,1:l1,1) = cc(1,1,1:l1) + cc(1,2,1:l1)
    ch(1,1:l1,2) = cc(1,1,1:l1) - cc(1,2,1:l1)
    ch(2,1:l1,1) = cc(2,1,1:l1) + cc(2,2,1:l1)
    ch(2,1:l1,2) = cc(2,1,1:l1) - cc(2,2,1:l1)

  else

    do k = 1, l1
      do i = 2, ido, 2

        ch(i-1,k,1) = cc(i-1,1,k) + cc(i-1,2,k)
        tr2         = cc(i-1,1,k) - cc(i-1,2,k)
        ch(i,k,1)   = cc(i,1,k)   + cc(i,2,k)
        ti2         = cc(i,1,k)   - cc(i,2,k)

        ch(i,k,2)   = wa1(i-1) * ti2 + wa1(i) * tr2
        ch(i-1,k,2) = wa1(i-1) * tr2 - wa1(i) * ti2

      end do
    end do

  end if

  return
end
subroutine passb3 ( ido, l1, cc, ch, wa1, wa2 )

!*****************************************************************************80
!
!! PASSB3 is a lower level routine used by CFFTB1.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) ido
  integer ( kind = 4 ) l1

  real    ( kind = 4 ) cc(ido,3,l1)
  real    ( kind = 4 ) ch(ido,l1,3)
  real    ( kind = 4 ) ci2
  real    ( kind = 4 ) ci3
  real    ( kind = 4 ) cr2
  real    ( kind = 4 ) cr3
  real    ( kind = 4 ) di2
  real    ( kind = 4 ) di3
  real    ( kind = 4 ) dr2
  real    ( kind = 4 ) dr3
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  real    ( kind = 4 ) taui
  real    ( kind = 4 ), parameter :: taur = -0.5E+00
  real    ( kind = 4 ) ti2
  real    ( kind = 4 ) tr2
  real    ( kind = 4 ) wa1(ido)
  real    ( kind = 4 ) wa2(ido)

  taui = sqrt ( 3.0E+00 ) / 2.0E+00

  if ( ido == 2 ) then

    do k = 1, l1

      tr2 = cc(1,2,k) + cc(1,3,k)
      cr2 = cc(1,1,k) + taur * tr2
      ch(1,k,1) = cc(1,1,k) + tr2

      ti2 = cc(2,2,k) + cc(2,3,k)
      ci2 = cc(2,1,k) + taur * ti2
      ch(2,k,1) = cc(2,1,k) + ti2

      cr3 = taui * ( cc(1,2,k) - cc(1,3,k) )
      ci3 = taui * ( cc(2,2,k) - cc(2,3,k) )

      ch(1,k,2) = cr2 - ci3
      ch(1,k,3) = cr2 + ci3
      ch(2,k,2) = ci2 + cr3
      ch(2,k,3) = ci2 - cr3

    end do

  else

    do k = 1, l1
      do i = 2, ido, 2

        tr2 = cc(i-1,2,k) + cc(i-1,3,k)
        cr2 = cc(i-1,1,k) + taur * tr2
        ch(i-1,k,1) = cc(i-1,1,k) + tr2

        ti2 = cc(i,2,k) + cc(i,3,k)
        ci2 = cc(i,1,k) + taur * ti2
        ch(i,k,1) = cc(i,1,k) + ti2

        cr3 = taui * ( cc(i-1,2,k) - cc(i-1,3,k) )
        ci3 = taui * ( cc(i,2,k) - cc(i,3,k) )

        dr2 = cr2 - ci3
        dr3 = cr2 + ci3
        di2 = ci2 + cr3
        di3 = ci2 - cr3

        ch(i,k,2)   = wa1(i-1) * di2 + wa1(i) * dr2
        ch(i-1,k,2) = wa1(i-1) * dr2 - wa1(i) * di2
        ch(i,k,3)   = wa2(i-1) * di3 + wa2(i) * dr3
        ch(i-1,k,3) = wa2(i-1) * dr3 - wa2(i) * di3

      end do
    end do

  end if

  return
end
subroutine passb4 ( ido, l1, cc, ch, wa1, wa2, wa3 )

!*****************************************************************************80
!
!! PASSB4 is a lower level routine used by CFFTB1.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) ido
  integer ( kind = 4 ) l1

  real    ( kind = 4 ) cc(ido,4,l1)
  real    ( kind = 4 ) ch(ido,l1,4)
  real    ( kind = 4 ) ci2
  real    ( kind = 4 ) ci3
  real    ( kind = 4 ) ci4
  real    ( kind = 4 ) cr2
  real    ( kind = 4 ) cr3
  real    ( kind = 4 ) cr4
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  real    ( kind = 4 ) ti1
  real    ( kind = 4 ) ti2
  real    ( kind = 4 ) ti3
  real    ( kind = 4 ) ti4
  real    ( kind = 4 ) tr1
  real    ( kind = 4 ) tr2
  real    ( kind = 4 ) tr3
  real    ( kind = 4 ) tr4
  real    ( kind = 4 ) wa1(ido)
  real    ( kind = 4 ) wa2(ido)
  real    ( kind = 4 ) wa3(ido)

  if ( ido == 2 ) then

    do k = 1, l1

      ti1 = cc(2,1,k) - cc(2,3,k)
      ti2 = cc(2,1,k) + cc(2,3,k)
      tr4 = cc(2,4,k) - cc(2,2,k)
      ti3 = cc(2,2,k) + cc(2,4,k)
      tr1 = cc(1,1,k) - cc(1,3,k)
      tr2 = cc(1,1,k) + cc(1,3,k)
      ti4 = cc(1,2,k) - cc(1,4,k)
      tr3 = cc(1,2,k) + cc(1,4,k)

      ch(1,k,1) = tr2 + tr3
      ch(1,k,3) = tr2 - tr3
      ch(2,k,1) = ti2 + ti3
      ch(2,k,3) = ti2 - ti3
      ch(1,k,2) = tr1 + tr4
      ch(1,k,4) = tr1 - tr4
      ch(2,k,2) = ti1 + ti4
      ch(2,k,4) = ti1 - ti4

    end do

  else

    do k = 1, l1
      do i = 2, ido, 2

        ti1 = cc(i,1,k) - cc(i,3,k)
        ti2 = cc(i,1,k) + cc(i,3,k)
        ti3 = cc(i,2,k) + cc(i,4,k)
        tr4 = cc(i,4,k) - cc(i,2,k)

        tr1 = cc(i-1,1,k) - cc(i-1,3,k)
        tr2 = cc(i-1,1,k) + cc(i-1,3,k)
        ti4 = cc(i-1,2,k) - cc(i-1,4,k)
        tr3 = cc(i-1,2,k) + cc(i-1,4,k)

        ch(i-1,k,1) = tr2 + tr3
        cr3 = tr2 - tr3
        ch(i,k,1) = ti2 + ti3
        ci3 = ti2 - ti3

        cr2 = tr1 + tr4
        cr4 = tr1 - tr4
        ci2 = ti1 + ti4
        ci4 = ti1 - ti4

        ch(i-1,k,2) = wa1(i-1) * cr2 - wa1(i) * ci2
        ch(i,k,2)   = wa1(i-1) * ci2 + wa1(i) * cr2
        ch(i-1,k,3) = wa2(i-1) * cr3 - wa2(i) * ci3
        ch(i,k,3)   = wa2(i-1) * ci3 + wa2(i) * cr3
        ch(i-1,k,4) = wa3(i-1) * cr4 - wa3(i) * ci4
        ch(i,k,4)   = wa3(i-1) * ci4 + wa3(i) * cr4

      end do
    end do

  end if

  return
end
subroutine passb5 ( ido, l1, cc, ch, wa1, wa2, wa3, wa4 )

!*****************************************************************************80
!
!! PASSB5 is a lower level routine used by CFFTB1.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) ido
  integer ( kind = 4 ) l1

  real    ( kind = 4 ) cc(ido,5,l1)
  real    ( kind = 4 ) ch(ido,l1,5)
  real    ( kind = 4 ) ci2
  real    ( kind = 4 ) ci3
  real    ( kind = 4 ) ci4
  real    ( kind = 4 ) ci5
  real    ( kind = 4 ) cr2
  real    ( kind = 4 ) cr3
  real    ( kind = 4 ) cr4
  real    ( kind = 4 ) cr5
  real    ( kind = 4 ) di2
  real    ( kind = 4 ) di3
  real    ( kind = 4 ) di4
  real    ( kind = 4 ) di5
  real    ( kind = 4 ) dr2
  real    ( kind = 4 ) dr3
  real    ( kind = 4 ) dr4
  real    ( kind = 4 ) dr5
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  real    ( kind = 4 ), parameter :: ti11 = 0.951056516295154E+00
  real    ( kind = 4 ), parameter :: ti12 = 0.587785252292473E+00
  real    ( kind = 4 ) ti2
  real    ( kind = 4 ) ti3
  real    ( kind = 4 ) ti4
  real    ( kind = 4 ) ti5
  real    ( kind = 4 ), parameter :: tr11 = 0.309016994374947E+00
  real    ( kind = 4 ), parameter :: tr12 = -0.809016994374947E+00
  real    ( kind = 4 ) tr2
  real    ( kind = 4 ) tr3
  real    ( kind = 4 ) tr4
  real    ( kind = 4 ) tr5
  real    ( kind = 4 ) wa1(ido)
  real    ( kind = 4 ) wa2(ido)
  real    ( kind = 4 ) wa3(ido)
  real    ( kind = 4 ) wa4(ido)

  if ( ido == 2 ) then

    do k = 1, l1

      ti5 = cc(2,2,k) - cc(2,5,k)
      ti2 = cc(2,2,k) + cc(2,5,k)
      ti4 = cc(2,3,k) - cc(2,4,k)
      ti3 = cc(2,3,k) + cc(2,4,k)
      tr5 = cc(1,2,k) - cc(1,5,k)
      tr2 = cc(1,2,k) + cc(1,5,k)
      tr4 = cc(1,3,k) - cc(1,4,k)
      tr3 = cc(1,3,k) + cc(1,4,k)

      ch(1,k,1) = cc(1,1,k) + tr2 + tr3
      ch(2,k,1) = cc(2,1,k) + ti2 + ti3

      cr2 = cc(1,1,k) + tr11 * tr2 + tr12 * tr3
      ci2 = cc(2,1,k) + tr11 * ti2 + tr12 * ti3
      cr3 = cc(1,1,k) + tr12 * tr2 + tr11 * tr3
      ci3 = cc(2,1,k) + tr12 * ti2 + tr11 * ti3

      cr5 = ti11 * tr5 + ti12 * tr4
      ci5 = ti11 * ti5 + ti12 * ti4
      cr4 = ti12 * tr5 - ti11 * tr4
      ci4 = ti12 * ti5 - ti11 * ti4

      ch(1,k,2) = cr2 - ci5
      ch(1,k,5) = cr2 + ci5
      ch(2,k,2) = ci2 + cr5
      ch(2,k,3) = ci3 + cr4
      ch(1,k,3) = cr3 - ci4
      ch(1,k,4) = cr3 + ci4
      ch(2,k,4) = ci3 - cr4
      ch(2,k,5) = ci2 - cr5

    end do

  else

    do k = 1, l1
      do i = 2, ido, 2

        ti5 = cc(i,2,k) - cc(i,5,k)
        ti2 = cc(i,2,k) + cc(i,5,k)
        ti4 = cc(i,3,k) - cc(i,4,k)
        ti3 = cc(i,3,k) + cc(i,4,k)

        tr5 = cc(i-1,2,k) - cc(i-1,5,k)
        tr2 = cc(i-1,2,k) + cc(i-1,5,k)
        tr4 = cc(i-1,3,k) - cc(i-1,4,k)
        tr3 = cc(i-1,3,k) + cc(i-1,4,k)

        ch(i-1,k,1) = cc(i-1,1,k) + tr2 + tr3
        ch(i,k,1)   = cc(i,1,k)   + ti2 + ti3

        cr2 = cc(i-1,1,k) + tr11 * tr2 + tr12 * tr3
        ci2 = cc(i,1,k)   + tr11 * ti2 + tr12 * ti3
        cr3 = cc(i-1,1,k) + tr12 * tr2 + tr11 * tr3
        ci3 = cc(i,1,k)   + tr12 * ti2 + tr11 * ti3

        cr5 = ti11 * tr5 + ti12 * tr4
        ci5 = ti11 * ti5 + ti12 * ti4
        cr4 = ti12 * tr5 - ti11 * tr4
        ci4 = ti12 * ti5 - ti11 * ti4

        dr3 = cr3 - ci4
        dr4 = cr3 + ci4
        di3 = ci3 + cr4
        di4 = ci3 - cr4
        dr5 = cr2 + ci5
        dr2 = cr2 - ci5
        di5 = ci2 - cr5
        di2 = ci2 + cr5

        ch(i-1,k,2) = wa1(i-1) * dr2 - wa1(i) * di2
        ch(i,k,2)   = wa1(i-1) * di2 + wa1(i) * dr2
        ch(i-1,k,3) = wa2(i-1) * dr3 - wa2(i) * di3
        ch(i,k,3)   = wa2(i-1) * di3 + wa2(i) * dr3
        ch(i-1,k,4) = wa3(i-1) * dr4 - wa3(i) * di4
        ch(i,k,4)   = wa3(i-1) * di4 + wa3(i) * dr4
        ch(i-1,k,5) = wa4(i-1) * dr5 - wa4(i) * di5
        ch(i,k,5)   = wa4(i-1) * di5 + wa4(i) * dr5

      end do
    end do

  end if

  return
end
subroutine passf ( nac, ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa )

!*****************************************************************************80
!
!! PASSF is a lower level routine used by CFFTF1.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) idl1
  integer ( kind = 4 ) ido
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) l1

  real    ( kind = 4 ) c1(ido,l1,ip)
  real    ( kind = 4 )c2(idl1,ip)
  real    ( kind = 4 ) cc(ido,ip,l1)
  real    ( kind = 4 ) ch(ido,l1,ip)
  real    ( kind = 4 ) ch2(idl1,ip)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) idij
  integer ( kind = 4 ) idj
  integer ( kind = 4 ) idl
  integer ( kind = 4 ) idlj
  integer ( kind = 4 ) idp
  integer ( kind = 4 ) ik
  integer ( kind = 4 ) inc
  integer ( kind = 4 ) ipph
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jc
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) lc
  integer ( kind = 4 ) nac
  integer ( kind = 4 ) nt
  real    ( kind = 4 ) wa(*)
  real    ( kind = 4 ) wai
  real    ( kind = 4 ) war

  nt = ip * idl1
  ipph = (ip+1) / 2
  idp = ip * ido

  if ( l1 <= ido ) then

    do j = 2, ipph
      jc = ip + 2 - j
      ch(1:ido,1:l1,j)  = cc(1:ido,j,1:l1) + cc(1:ido,jc,1:l1)
      ch(1:ido,1:l1,jc) = cc(1:ido,j,1:l1) - cc(1:ido,jc,1:l1)
    end do

    ch(1:ido,1:l1,1) = cc(1:ido,1,1:l1)

  else

    do j = 2, ipph
      jc = ip + 2 - j
      ch(1:ido,1:l1,j)  = cc(1:ido,j,1:l1) + cc(1:ido,jc,1:l1)
      ch(1:ido,1:l1,jc) = cc(1:ido,j,1:l1) - cc(1:ido,jc,1:l1)
    end do

    ch(1:ido,1:l1,1) = cc(1:ido,1,1:l1)

  end if

  idl = 2 - ido
  inc = 0

  do l = 2, ipph

    lc = ip + 2 - l
    idl = idl + ido

    do ik = 1, idl1
      c2(ik,l)  = ch2(ik,1) + wa(idl-1) * ch2(ik,2)
      c2(ik,lc) =           - wa(idl)   * ch2(ik,ip)
    end do

    idlj = idl
    inc = inc + ido

    do j = 3, ipph

      jc = ip + 2 - j

      idlj = idlj + inc
      if ( idp < idlj ) then
        idlj = idlj - idp
      end if

      war = wa(idlj-1)
      wai = wa(idlj)

      do ik = 1, idl1
        c2(ik,l)  = c2(ik,l)  + war * ch2(ik,j)
        c2(ik,lc) = c2(ik,lc) - wai * ch2(ik,jc)
      end do

    end do

  end do

  do j = 2, ipph
    ch2(1:idl1,1) = ch2(1:idl1,1) + ch2(1:idl1,j)
  end do

  do j = 2, ipph
    jc = ip + 2 - j
    do ik = 2, idl1, 2
      ch2(ik-1,j)  = c2(ik-1,j) - c2(ik,jc)
      ch2(ik-1,jc) = c2(ik-1,j) + c2(ik,jc)
      ch2(ik,j)    = c2(ik,j)   + c2(ik-1,jc)
      ch2(ik,jc)   = c2(ik,j)   - c2(ik-1,jc)
    end do
  end do

  if ( ido == 2 ) then
    nac = 1
    return
  end if

  nac = 0

  c2(1:idl1,1)    = ch2(1:idl1,1)
  c1(1,1:l1,2:ip) = ch(1,1:l1,2:ip)
  c1(2,1:l1,2:ip) = ch(2,1:l1,2:ip)

  if ( ( ido / 2 ) <= l1 ) then

    idij = 0
    do j = 2, ip
      idij = idij + 2
      do i = 4, ido, 2
        idij = idij + 2
        c1(i-1,1:l1,j) = wa(idij-1) * ch(i-1,1:l1,j) + wa(idij) * ch(i,1:l1,j)
        c1(i,1:l1,j)   = wa(idij-1) * ch(i,1:l1,j)   - wa(idij) * ch(i-1,1:l1,j)
      end do
    end do

  else

    idj = 2 - ido

    do j = 2, ip
      idj = idj + ido
      do k = 1, l1
        idij = idj
        do i = 4, ido, 2
          idij = idij + 2
          c1(i-1,k,j) = wa(idij-1) * ch(i-1,k,j) + wa(idij) * ch(i,k,j)
          c1(i,k,j)   = wa(idij-1) * ch(i,k,j)   - wa(idij) * ch(i-1,k,j)
        end do
      end do
    end do

  end if

  return
end
subroutine passf2 ( ido, l1, cc, ch, wa1 )

!*****************************************************************************80
!
!! PASSF2 is a lower level routine used by CFFTF1.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) ido
  integer ( kind = 4 ) l1

  real    ( kind = 4 ) cc(ido,2,l1)
  real    ( kind = 4 ) ch(ido,l1,2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  real    ( kind = 4 ) ti2
  real    ( kind = 4 ) tr2
  real    ( kind = 4 ) wa1(ido)

  if ( ido <= 2 ) then

    ch(1,1:l1,1) = cc(1,1,1:l1) + cc(1,2,1:l1)
    ch(1,1:l1,2) = cc(1,1,1:l1) - cc(1,2,1:l1)
    ch(2,1:l1,1) = cc(2,1,1:l1) + cc(2,2,1:l1)
    ch(2,1:l1,2) = cc(2,1,1:l1) - cc(2,2,1:l1)

  else

    do k = 1, l1
      do i = 2, ido, 2

        ch(i-1,k,1) = cc(i-1,1,k) + cc(i-1,2,k)
        tr2         = cc(i-1,1,k) - cc(i-1,2,k)

        ch(i,k,1) = cc(i,1,k) + cc(i,2,k)
        ti2       = cc(i,1,k) - cc(i,2,k)

        ch(i,k,2)   = wa1(i-1) * ti2 - wa1(i) * tr2
        ch(i-1,k,2) = wa1(i-1) * tr2 + wa1(i) * ti2

      end do
    end do

  end if

  return
end
subroutine passf3 ( ido, l1, cc, ch, wa1, wa2 )

!*****************************************************************************80
!
!! PASSF3 is a lower level routine used by CFFTF1.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) ido
  integer ( kind = 4 ) l1

  real    ( kind = 4 ) cc(ido,3,l1)
  real    ( kind = 4 ) ch(ido,l1,3)
  real    ( kind = 4 ) ci2
  real    ( kind = 4 ) ci3
  real    ( kind = 4 ) cr2
  real    ( kind = 4 ) cr3
  real    ( kind = 4 ) di2
  real    ( kind = 4 ) di3
  real    ( kind = 4 ) dr2
  real    ( kind = 4 ) dr3
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  real    ( kind = 4 ) taui
  real    ( kind = 4 ), parameter :: taur = -0.5E+00
  real    ( kind = 4 ) ti2
  real    ( kind = 4 ) tr2
  real    ( kind = 4 ) wa1(ido)
  real    ( kind = 4 ) wa2(ido)

  taui = - sqrt ( 3.0E+00 ) / 2.0E+00

  if ( ido == 2 ) then

    do k = 1, l1

      tr2 = cc(1,2,k) + cc(1,3,k)
      cr2 = cc(1,1,k) + taur * tr2
      ch(1,k,1) = cc(1,1,k) + tr2

      ti2 = cc(2,2,k) + cc(2,3,k)
      ci2 = cc(2,1,k) + taur * ti2
      ch(2,k,1) = cc(2,1,k) + ti2

      cr3 = taui * ( cc(1,2,k) - cc(1,3,k) )
      ci3 = taui * ( cc(2,2,k) - cc(2,3,k) )

      ch(1,k,2) = cr2 - ci3
      ch(1,k,3) = cr2 + ci3
      ch(2,k,2) = ci2 + cr3
      ch(2,k,3) = ci2 - cr3

    end do

  else

    do k = 1, l1
      do i = 2, ido, 2

        tr2 = cc(i-1,2,k) + cc(i-1,3,k)
        cr2 = cc(i-1,1,k) + taur * tr2
        ch(i-1,k,1) = cc(i-1,1,k) + tr2

        ti2 = cc(i,2,k) + cc(i,3,k)
        ci2 = cc(i,1,k) + taur * ti2
        ch(i,k,1) = cc(i,1,k) + ti2

        cr3 = taui * ( cc(i-1,2,k) - cc(i-1,3,k) )
        ci3 = taui * ( cc(i,2,k)   - cc(i,3,k) )

        dr2 = cr2 - ci3
        dr3 = cr2 + ci3
        di2 = ci2 + cr3
        di3 = ci2 - cr3

        ch(i,k,2)   = wa1(i-1) * di2 - wa1(i) * dr2
        ch(i-1,k,2) = wa1(i-1) * dr2 + wa1(i) * di2
        ch(i,k,3)   = wa2(i-1) * di3 - wa2(i) * dr3
        ch(i-1,k,3) = wa2(i-1) * dr3 + wa2(i) * di3

      end do
    end do

  end if

  return
end
subroutine passf4 ( ido, l1, cc, ch, wa1, wa2, wa3 )

!*****************************************************************************80
!
!! PASSF4 is a lower level routine used by CFFTF1.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) ido
  integer ( kind = 4 ) l1

  real    ( kind = 4 ) cc(ido,4,l1)
  real    ( kind = 4 ) ch(ido,l1,4)
  real    ( kind = 4 ) ci2
  real    ( kind = 4 ) ci3
  real    ( kind = 4 ) ci4
  real    ( kind = 4 ) cr2
  real    ( kind = 4 ) cr3
  real    ( kind = 4 ) cr4
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  real    ( kind = 4 ) ti1
  real    ( kind = 4 ) ti2
  real    ( kind = 4 ) ti3
  real    ( kind = 4 ) ti4
  real    ( kind = 4 ) tr1
  real    ( kind = 4 ) tr2
  real    ( kind = 4 ) tr3
  real    ( kind = 4 ) tr4
  real    ( kind = 4 ) wa1(ido)
  real    ( kind = 4 ) wa2(ido)
  real    ( kind = 4 ) wa3(ido)

  if ( ido == 2 ) then

    do k = 1, l1

      ti1 = cc(2,1,k) - cc(2,3,k)
      ti2 = cc(2,1,k) + cc(2,3,k)
      tr4 = cc(2,2,k) - cc(2,4,k)
      ti3 = cc(2,2,k) + cc(2,4,k)
      tr1 = cc(1,1,k) - cc(1,3,k)
      tr2 = cc(1,1,k) + cc(1,3,k)
      ti4 = cc(1,4,k) - cc(1,2,k)
      tr3 = cc(1,2,k) + cc(1,4,k)

      ch(1,k,1) = tr2 + tr3
      ch(1,k,3) = tr2 - tr3
      ch(2,k,1) = ti2 + ti3
      ch(2,k,3) = ti2 - ti3
      ch(1,k,2) = tr1 + tr4
      ch(1,k,4) = tr1 - tr4
      ch(2,k,2) = ti1 + ti4
      ch(2,k,4) = ti1 - ti4

    end do

  else

    do k = 1, l1
      do i = 2, ido, 2

        ti1 = cc(i,1,k)   - cc(i,3,k)
        ti2 = cc(i,1,k)   + cc(i,3,k)
        ti3 = cc(i,2,k)   + cc(i,4,k)
        tr4 = cc(i,2,k)   - cc(i,4,k)
        tr1 = cc(i-1,1,k) - cc(i-1,3,k)
        tr2 = cc(i-1,1,k) + cc(i-1,3,k)
        ti4 = cc(i-1,4,k) - cc(i-1,2,k)
        tr3 = cc(i-1,2,k) + cc(i-1,4,k)

        ch(i-1,k,1) = tr2 + tr3
        cr3         = tr2 - tr3
        ch(i,k,1)   = ti2 + ti3
        ci3         = ti2 - ti3

        cr2 = tr1 + tr4
        cr4 = tr1 - tr4
        ci2 = ti1 + ti4
        ci4 = ti1 - ti4

        ch(i-1,k,2) = wa1(i-1) * cr2 + wa1(i) * ci2
        ch(i,k,2)   = wa1(i-1) * ci2 - wa1(i) * cr2
        ch(i-1,k,3) = wa2(i-1) * cr3 + wa2(i) * ci3
        ch(i,k,3)   = wa2(i-1) * ci3 - wa2(i) * cr3
        ch(i-1,k,4) = wa3(i-1) * cr4 + wa3(i) * ci4
        ch(i,k,4)   = wa3(i-1) * ci4 - wa3(i) * cr4

      end do
    end do

  end if

  return
end
subroutine passf5 ( ido, l1, cc, ch, wa1, wa2, wa3, wa4 )

!*****************************************************************************80
!
!! PASSF5 is a lower level routine used by CFFTF1.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) ido
  integer ( kind = 4 ) l1

  real    ( kind = 4 ) cc(ido,5,l1)
  real    ( kind = 4 ) ch(ido,l1,5)
  real    ( kind = 4 ) ci2
  real    ( kind = 4 ) ci3
  real    ( kind = 4 ) ci4
  real    ( kind = 4 ) ci5
  real    ( kind = 4 ) cr2
  real    ( kind = 4 ) cr3
  real    ( kind = 4 ) cr4
  real    ( kind = 4 ) cr5
  real    ( kind = 4 ) di2
  real    ( kind = 4 ) di3
  real    ( kind = 4 ) di4
  real    ( kind = 4 ) di5
  real    ( kind = 4 ) dr2
  real    ( kind = 4 ) dr3
  real    ( kind = 4 ) dr4
  real    ( kind = 4 ) dr5
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  real    ( kind = 4 ), parameter :: ti11 = -0.951056516295154E+00
  real    ( kind = 4 ), parameter :: ti12 = -0.587785252292473E+00
  real    ( kind = 4 ) ti2
  real    ( kind = 4 ) ti3
  real    ( kind = 4 ) ti4
  real    ( kind = 4 ) ti5
  real    ( kind = 4 ) tr2
  real    ( kind = 4 ) tr3
  real    ( kind = 4 ) tr4
  real    ( kind = 4 ) tr5
!
!  cos ( 72 ) = +0.3090
!
  real    ( kind = 4 ), parameter :: tr11 =  0.309016994374947E+00
!
!  cos ( 36 ) = +0.809016
!
  real    ( kind = 4 ), parameter :: tr12 = -0.809016994374947E+00
  real    ( kind = 4 ) wa1(ido)
  real    ( kind = 4 ) wa2(ido)
  real    ( kind = 4 ) wa3(ido)
  real    ( kind = 4 ) wa4(ido)

  if ( ido == 2 ) then

    do k = 1, l1

      ti5 = cc(2,2,k) - cc(2,5,k)
      ti2 = cc(2,2,k) + cc(2,5,k)
      ti4 = cc(2,3,k) - cc(2,4,k)
      ti3 = cc(2,3,k) + cc(2,4,k)
      tr5 = cc(1,2,k) - cc(1,5,k)
      tr2 = cc(1,2,k) + cc(1,5,k)
      tr4 = cc(1,3,k) - cc(1,4,k)
      tr3 = cc(1,3,k) + cc(1,4,k)

      ch(1,k,1) = cc(1,1,k) + tr2 + tr3
      ch(2,k,1) = cc(2,1,k) + ti2 + ti3

      cr2 = cc(1,1,k) + tr11 * tr2 + tr12 * tr3
      ci2 = cc(2,1,k) + tr11 * ti2 + tr12 * ti3
      cr3 = cc(1,1,k) + tr12 * tr2 + tr11 * tr3
      ci3 = cc(2,1,k) + tr12 * ti2 + tr11 * ti3

      cr5 = ti11 * tr5 + ti12 * tr4
      ci5 = ti11 * ti5 + ti12 * ti4
      cr4 = ti12 * tr5 - ti11 * tr4
      ci4 = ti12 * ti5 - ti11 * ti4

      ch(1,k,2) = cr2 - ci5
      ch(1,k,5) = cr2 + ci5
      ch(2,k,2) = ci2 + cr5
      ch(2,k,3) = ci3 + cr4
      ch(1,k,3) = cr3 - ci4
      ch(1,k,4) = cr3 + ci4
      ch(2,k,4) = ci3 - cr4
      ch(2,k,5) = ci2 - cr5

    end do

  else

    do k = 1, l1
      do i = 2, ido, 2

        ti5 = cc(i,2,k) - cc(i,5,k)
        ti2 = cc(i,2,k) + cc(i,5,k)
        ti4 = cc(i,3,k) - cc(i,4,k)
        ti3 = cc(i,3,k) + cc(i,4,k)

        tr5 = cc(i-1,2,k) - cc(i-1,5,k)
        tr2 = cc(i-1,2,k) + cc(i-1,5,k)
        tr4 = cc(i-1,3,k) - cc(i-1,4,k)
        tr3 = cc(i-1,3,k) + cc(i-1,4,k)

        ch(i-1,k,1) = cc(i-1,1,k) + tr2 + tr3
        ch(i,k,1)   = cc(i,1,k)   + ti2 + ti3

        cr2 = cc(i-1,1,k) + tr11 * tr2 + tr12 * tr3
        ci2 = cc(i,1,k)   + tr11 * ti2 + tr12 * ti3
        cr3 = cc(i-1,1,k) + tr12 * tr2 + tr11 * tr3
        ci3 = cc(i,1,k)   + tr12 * ti2 + tr11 * ti3

        cr5 = ti11 * tr5 + ti12 * tr4
        ci5 = ti11 * ti5 + ti12 * ti4
        cr4 = ti12 * tr5 - ti11 * tr4
        ci4 = ti12 * ti5 - ti11 * ti4

        dr3 = cr3 - ci4
        dr4 = cr3 + ci4
        di3 = ci3 + cr4
        di4 = ci3 - cr4
        dr5 = cr2 + ci5
        dr2 = cr2 - ci5
        di5 = ci2 - cr5
        di2 = ci2 + cr5

        ch(i-1,k,2) = wa1(i-1) * dr2 + wa1(i) * di2
        ch(i,k,2)   = wa1(i-1) * di2 - wa1(i) * dr2
        ch(i-1,k,3) = wa2(i-1) * dr3 + wa2(i) * di3
        ch(i,k,3)   = wa2(i-1) * di3 - wa2(i) * dr3
        ch(i-1,k,4) = wa3(i-1) * dr4 + wa3(i) * di4
        ch(i,k,4)   = wa3(i-1) * di4 - wa3(i) * dr4
        ch(i-1,k,5) = wa4(i-1) * dr5 + wa4(i) * di5
        ch(i,k,5)   = wa4(i-1) * di5 - wa4(i) * dr5

      end do
    end do

  end if

  return
end
function r4_cas ( x )

!*****************************************************************************80
!
!! R4_CAS returns the "casine" of an R4.
!
!  Discussion:
!
!    The "casine", used in the discrete Hartley transform, is abbreviated
!    CAS(X), and defined by:
!
!      CAS(X) = cos ( X ) + sin( X )
!             = sqrt ( 2 ) * sin ( X + pi/4 )
!             = sqrt ( 2 ) * cos ( X - pi/4 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 January 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 4 ) X, the number whose casine is desired.
!
!    Output, real ( kind = 4 ) R4_CAS, the casine of X, which will be between
!    plus or minus the square root of 2.
!
  implicit none

  real    ( kind = 4 ) r4_cas
  real    ( kind = 4 ) x

  r4_cas = cos ( x ) + sin ( x )

  return
end
subroutine r4_swap ( x, y )

!*****************************************************************************80
!
!! R4_SWAP swaps two R4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 May 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, real ( kind = 4 ) X, Y.  On output, the values of X and
!    Y have been interchanged.
!
  implicit none

  real    ( kind = 4 ) x
  real    ( kind = 4 ) y
  real    ( kind = 4 ) z

  z = x
  x = y
  y = z

  return
end
function r4_uniform ( a, b, seed )

!*****************************************************************************80
!
!! R4_UNIFORM returns a scaled pseudorandom R4.
!
!  Discussion:
!
!    An R4 is a real ( kind = 4 ) value.
!
!    The pseudorandom number should be uniformly distributed
!    between A and B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 4 ) A, B, the limits of the interval.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 4 ) R4_UNIFORM, a number strictly between A and B.
!
  implicit none

  real    ( kind = 4 ) a
  real    ( kind = 4 ) b
  integer ( kind = 4 ) k
  real    ( kind = 4 ) r4_uniform
  integer ( kind = 4 ) seed

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4_UNIFORM - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop
  end if

  k = seed / 127773

  seed = 16807 * ( seed - k * 127773 ) - k * 2836

  if ( seed < 0 ) then
    seed = seed + 2147483647
  end if

  r4_uniform = a + ( b - a ) * real ( seed, kind = 4 ) * 4.656612875E-10

  return
end
subroutine r4_sct ( n, x, y )

!*****************************************************************************80
!
!! R4_SCT computes a real "slow" cosine transform.
!
!  Discussion:
!
!    This routine is provided for illustration and testing.  It is inefficient
!    relative to optimized routines that use fast Fourier techniques.
!
!      Y(1) = Sum ( 1 <= J <= N ) X(J)
!
!      For 2 <= I <= N-1:
!
!        Y(I) = 2 * Sum ( 1 <= J <= N ) X(J)
!          * cos ( PI * ( I - 1 ) * ( J - 1 ) / ( N - 1 ) )
!
!      Y(N) = Sum ( X(1:N:2) ) - Sum ( X(2:N:2) )
!
!    Applying the routine twice in succession should yield the original data,
!    multiplied by 2 * ( N + 1 ).  This is a good check for correctness
!    and accuracy.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 January 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of data values.
!
!    Input, real ( kind = 4 ) X(N), the data sequence.
!
!    Output, real ( kind = 4 ) Y(N), the transformed data.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real    ( kind = 4 ), parameter :: pi = 3.1415926E+00
  real    ( kind = 4 ) theta
  real    ( kind = 4 ) x(n)
  real    ( kind = 4 ) y(n)

  y(1) = sum ( x(1:n) )

  do i = 2, n-1
    y(i) = 0.0E+00
    do j = 1, n
      theta = pi * &
        real ( mod ( ( j - 1 ) * ( i - 1 ), 2 * ( n - 1 ) ), kind = 4 ) &
       / real ( n - 1, kind = 4 )
      y(i) = y(i) + 2.0E+00 * x(j) * cos ( theta )
    end do
  end do

  y(n) = sum ( x(1:n:2) ) - sum ( x(2:n:2) )

  return
end
subroutine r4_sftb ( n, r, azero, a, b )

!*****************************************************************************80
!
!! R4_SFTB computes a "slow" backward Fourier transform of real data.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of data values.
!
!    Output, real ( kind = 4 ) R(N), the reconstructed data sequence.
!
!    Input, real ( kind = 4 ) AZERO, the constant Fourier coefficient.
!
!    Input, real ( kind = 4 ) A(N/2), B(N/2), the Fourier coefficients.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 4 ) a(n/2)
  real    ( kind = 4 ) azero
  real    ( kind = 4 ) b(n/2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  real    ( kind = 4 ), parameter :: pi = 3.1415926E+00
  real    ( kind = 4 ) r(n)
  real    ( kind = 4 ) theta

  r(1:n) = azero
  do i = 1, n
    do k = 1, n/2
      theta = real ( k * ( i - 1 ) * 2, kind = 4 ) * pi &
        / real ( n, kind = 4 )
      r(i) = r(i) + a(k) * cos ( theta ) + b(k) * sin ( theta )
    end do
  end do

  return
end
subroutine r4_sftf ( n, r, azero, a, b )

!*****************************************************************************80
!
!! R4_SFTF computes a "slow" forward Fourier transform of real data.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    13 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of data values.
!
!    Input, real ( kind = 4 ) R(N), the data to be transformed.
!
!    Output, real ( kind = 4 ) AZERO, = sum ( 1 <= I <= N ) R(I) / N.
!
!    Output, real ( kind = 4 ) A(N/2), B(N/2), the Fourier coefficients.
!
  implicit none

  integer n

  real    ( kind = 4 ) a(1:n/2)
  real    ( kind = 4 ) azero
  real    ( kind = 4 ) b(1:n/2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real    ( kind = 4 ), parameter :: pi = 3.1415926E+00
  real    ( kind = 4 ) r(n)
  real    ( kind = 4 ) theta

  azero = sum ( r(1:n) ) / real ( n, kind = 4 )

  do i = 1, n / 2

    a(i) = 0.0E+00
    b(i) = 0.0E+00

    do j = 1, n
      theta = real ( 2 * i * ( j - 1 ), kind = 4 ) * pi / real ( n, kind = 4 )
      a(i) = a(i) + r(j) * cos ( theta )
      b(i) = b(i) + r(j) * sin ( theta )
    end do

    a(i) = a(i) / real ( n, kind = 4 )
    b(i) = b(i) / real ( n, kind = 4 )

    if ( i /= ( n / 2 ) ) then
      a(i) = 2.0E+00 * a(i)
      b(i) = 2.0E+00 * b(i)
    end if

  end do

  return
end
subroutine r4_sht ( n, a, b  )

!*****************************************************************************80
!
!! R4_SHT computes a "slow" Hartley transform of real data.
!
!  Discussion:
!
!    The discrete Hartley transform B of a set of data A is
!
!      B(I) = 1/sqrt(N) * Sum ( 0 <= J <= N-1 ) A(J) * CAS(2*PI*I*J/N)
!
!    Here, the data and coefficients are indexed from 0 to N-1.
!
!    With the above normalization factor of 1/sqrt(N), the Hartley
!    transform is its own inverse.
!
!    This routine is provided for illustration and testing.  It is inefficient
!    relative to optimized routines.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 January 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of data values.
!
!    Input, real ( kind = 4 ) A(0:N-1), the data to be transformed.
!
!    Output, real ( kind = 4 ) B(0:N-1), the transformed data.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 4 ) a(0:n-1)
  real    ( kind = 4 ) b(0:n-1)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real    ( kind = 4 ), parameter :: pi = 3.1415926E+00
  real    ( kind = 4 ) r4_cas
  real    ( kind = 4 ) theta

  b(0:n-1) = 0.0E+00

  do i = 0, n-1
    do j = 0, n-1
      theta = 2.0E+00 * pi * real ( mod ( i * j, n ), kind = 4 ) &
        / real ( n, kind = 4 )
      b(i) = b(i) + a(j) * r4_cas ( theta )
    end do
  end do

  b(0:n-1) = b(0:n-1) / sqrt ( real ( n, kind = 4 ) )

  return
end
subroutine r4_sst ( n, x, y )

!*****************************************************************************80
!
!! R4_SST computes a real "slow" sine transform.
!
!  Discussion:
!
!    This routine is provided for illustration and testing.  It is inefficient
!    relative to optimized routines that use fast Fourier techniques.
!
!    For 1 <= I <= N,
!
!      Y(I) = Sum ( 1 <= J <= N ) X(J) * sin ( PI * I * J / ( N + 1 ) )
!
!    Applying the routine twice in succession should yield the original data,
!    multiplied by N / 2.  This is a good check for correctness and accuracy.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 December 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of data values.
!
!    Input, real ( kind = 4 ) X(N), the data sequence.
!
!    Output, real ( kind = 4 ) Y(N), the transformed data.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real    ( kind = 4 ), parameter :: pi = 3.1415926E+00
  real    ( kind = 4 ) theta(n)
  real    ( kind = 4 ) x(n)
  real    ( kind = 4 ) y(n)

  call r4vec_indicator ( n, theta )
  theta(1:n) = theta(1:n) * pi / real ( n + 1, kind = 4 )

  y(1:n) = 0.0E+00

  do i = 1, n
    y(1:n) = y(1:n) + 2.0E+00 * x(i) * sin ( real ( i, kind = 4 ) * theta(1:n) )
  end do

  return
end
subroutine r4_sqctb ( n, x, y )

!*****************************************************************************80
!
!! R4_SQCTB computes a real "slow" quarter cosine transform backward.
!
!  Discussion:
!
!    This routine is provided for illustration and testing.  It is inefficient
!    relative to optimized routines that use fast Fourier techniques.
!
!    For 0 <= I <= N-1,
!
!      Y(I) = X(0) + 2 Sum ( 1 <= J <= N-1 ) X(J) * cos ( PI * J * (I+1/2) / N )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 January 2002
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Briggs, Van Emden Henson,
!    The DFT: An Owner's Manual for the Discrete Fourier Transform,
!    SIAM, 1995,
!    LC: QA403.5.B75,
!    ISBN13: 978-0-898713-42-8.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of data values.
!
!    Input, real ( kind = 4 ) X(0:N-1), the data sequence.
!
!    Output, real ( kind = 4 ) Y(0:N-1), the transformed data.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real    ( kind = 4 ), parameter :: pi = 3.1415926E+00
  real    ( kind = 4 ) theta
  real    ( kind = 4 ) x(0:n-1)
  real    ( kind = 4 ) y(0:n-1)

  y(0:n-1) = x(0)

  do i = 0, n-1
    do j = 1, n-1

      theta = 0.5E+00 * pi * real ( j * ( 2 * i + 1 ), kind = 4 ) &
        / real ( n, kind = 4 )
      y(i) = y(i) + 2.0E+00 * x(j) * cos ( theta  )

    end do

  end do

  return
end
subroutine r4_sqctf ( n, x, y )

!*****************************************************************************80
!
!! R4_SQCTF computes a real "slow" quarter cosine transform forward.
!
!  Discussion:
!
!    This routine is provided for illustration and testing.  It is inefficient
!    relative to optimized routines that use fast Fourier techniques.
!
!    For 0 <= I <= N-1,
!
!      Y(I) = (1/N) Sum ( 0 <= J <= N-1 ) X(J) * cos ( PI * I * (J+1/2) / N )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 January 2002
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Briggs, Van Emden Henson,
!    The DFT: An Owner's Manual for the Discrete Fourier Transform,
!    SIAM, 1995,
!    LC: QA403.5.B75,
!    ISBN13: 978-0-898713-42-8.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of data values.
!
!    Input, real ( kind = 4 ) X(0:N-1), the data sequence.
!
!    Output, real ( kind = 4 ) Y(0:N-1), the transformed data.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real    ( kind = 4 ), parameter :: pi = 3.1415926E+00
  real    ( kind = 4 ) theta
  real    ( kind = 4 ) x(0:n-1)
  real    ( kind = 4 ) y(0:n-1)

  y(0:n-1) = 0.0E+00

  do i = 0, n-1
    do j = 0, n-1
      theta = 0.5E+00 * pi * real ( i * ( 2 * j + 1 ), kind = 4 ) &
        / real ( n, kind = 4 )
      y(i) = y(i) + x(j) * cos ( theta  )
    end do
  end do

  y(0:n-1) = y(0:n-1) / real ( n, kind = 4 )

  return
end
subroutine r4_sqstb ( n, x, y )

!*****************************************************************************80
!
!! R4_SQSTB computes a real "slow" quarter sine transform backward.
!
!  Discussion:
!
!    This routine is provided for illustration and testing.  It is inefficient
!    relative to optimized routines that use fast Fourier techniques.
!
!    For 0 <= I <= N-1,
!
!      Y(I) = -2 Sum ( 1 <= J <= N-1 ) X(J) * sin ( PI * J * (I+1/2) / N )
!             - X(N) * cos ( pi * I )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 January 2002
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Briggs, Van Emden Henson,
!    The DFT: An Owner's Manual for the Discrete Fourier Transform,
!    SIAM, 1995,
!    LC: QA403.5.B75,
!    ISBN13: 978-0-898713-42-8.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of data values.
!
!    Input, real ( kind = 4 ) X(N), the data sequence.
!
!    Output, real ( kind = 4 ) Y(0:N-1), the transformed data.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real    ( kind = 4 ), parameter :: pi = 3.1415926E+00
  real    ( kind = 4 ) theta
  real    ( kind = 4 ) x(1:n)
  real    ( kind = 4 ) y(0:n-1)

  y(0:n-1) = 0.0E+00

  do i = 0, n-1
    do j = 1, n-1

      theta = 0.5E+00 * pi * real ( j * ( 2 * i + 1 ), kind = 4 ) &
        / real ( n, kind = 4 )
      y(i) = y(i) - 2.0E+00 * x(j) * sin ( theta  )

    end do

    theta = pi * real ( i, kind = 4 )
    y(i) = y(i) - x(n) * cos ( theta )

  end do

  return
end
subroutine r4_sqstf ( n, x, y )

!*****************************************************************************80
!
!! R4_SQSTF computes a real "slow" quarter sine transform forward.
!
!  Discussion:
!
!    This routine is provided for illustration and testing.  It is inefficient
!    relative to optimized routines that use fast Fourier techniques.
!
!    For 1 <= I <= N,
!
!      Y(I) = -(1/N) Sum ( 0 <= J <= N-1 ) X(J) * sin ( PI * I * (J+1/2) / N )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 January 2002
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    William Briggs, Van Emden Henson,
!    The DFT: An Owner's Manual for the Discrete Fourier Transform,
!    SIAM, 1995,
!    LC: QA403.5.B75,
!    ISBN13: 978-0-898713-42-8.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of data values.
!
!    Input, real ( kind = 4 ) X(0:N-1), the data sequence.
!
!    Output, real ( kind = 4 ) Y(N), the transformed data.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real    ( kind = 4 ), parameter :: pi = 3.1415926E+00
  real    ( kind = 4 ) theta
  real    ( kind = 4 ) x(0:n-1)
  real    ( kind = 4 ) y(n)

  y(1:n) = 0.0E+00

  do i = 1, n
    do j = 0, n-1
      theta = 0.5E+00 * pi * real ( i * ( 2 * j + 1 ), kind = 4 ) &
        / real ( n, kind = 4 )
      y(i) = y(i) + x(j) * sin ( theta  )
    end do
  end do

  y(1:n) = - y(1:n) / real ( n, kind = 4 )

  return
end
subroutine r4vec_indicator ( n, a )

!*****************************************************************************80
!
!! R4VEC_INDICATOR sets an R4VEC to the indicator vector.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements of A.
!
!    Output, real ( kind = 4 ) A(N), the array to be initialized.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 4 ) a(n)
  integer ( kind = 4 ) i

  do i = 1, n
    a(i) = real ( i, kind = 4 )
  end do

  return
end
subroutine r4vec_print_some ( n, a, max_print, title )

!*****************************************************************************80
!
!! R4VEC_PRINT_SOME prints "some" of an R4VEC.
!
!  Discussion:
!
!    The user specifies MAX_PRINT, the maximum number of lines to print.
!
!    If N, the size of the vector, is no more than MAX_PRINT, then
!    the entire vector is printed, one entry per line.
!
!    Otherwise, if possible, the first MAX_PRINT-2 entries are printed,
!    followed by a line of periods suggesting an omission,
!    and the last entry.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 September 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries of the vector.
!
!    Input, real ( kind = 4 ) A(N), the vector to be printed.
!
!    Input, integer ( kind = 4 ) MAX_PRINT, the maximum number of lines to print.
!
!    Input, character ( len = * ) TITLE, an optional title.
!
  implicit none

  integer   ( kind = 4 ) n

  real      ( kind = 4 ) a(n)
  integer   ( kind = 4 ) i
  integer   ( kind = 4 ) max_print
  character ( len = * ) title

  if ( max_print <= 0 ) then
    return
  end if

  if ( n <= 0 ) then
    return
  end if

  if ( 0 < len_trim ( title ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )
    write ( *, '(a)' ) ' '
  end if

  if ( n <= max_print ) then

    do i = 1, n
      write ( *, '(i8,2x,g14.6)' ) i, a(i)
    end do

  else if ( 3 <= max_print ) then

    do i = 1, max_print-2
      write ( *, '(i8,2x,g14.6)' ) i, a(i)
    end do
    write ( *, '(a)' ) '......  ..............'
    i = n
    write ( *, '(i8,2x,g14.6)' ) i, a(i)

  else

    do i = 1, max_print - 1
      write ( *, '(i8,2x,g14.6)' ) i, a(i)
    end do
    i = max_print
    write ( *, '(i8,2x,g14.6,2x,a)' ) i, a(i), '...more entries...'

  end if

  return
end
subroutine r4vec_reverse ( n, a )

!*****************************************************************************80
!
!! R4VEC_REVERSE reverses the elements of an R4VEC.
!
!  Example:
!
!    Input:
!
!      N = 5, A = ( 11.0, 12.0, 13.0, 14.0, 15.0 ).
!
!    Output:
!
!      A = ( 15.0, 14.0, 13.0, 12.0, 11.0 ).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 October 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the array.
!
!    Input/output, real ( kind = 4 ) A(N), the array to be reversed.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 4 ) a(n)
  integer ( kind = 4 ) i

  do i = 1, n/2
    call r4_swap ( a(i), a(n+1-i) )
  end do

  return
end
subroutine r4vec_uniform ( n, a, b, seed, r )

!*****************************************************************************80
!
!! R4VEC_UNIFORM returns a scaled pseudorandom R4VEC.
!
!  Discussion:
!
!    An R4VEC is a vector of R4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 March 2005
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of entries in the vector.
!
!    Input, real ( kind = 4 ) A, B, the lower and upper limits.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 4 ) R(N), the vector of pseudorandom values.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 4 ) a
  real    ( kind = 4 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real    ( kind = 4 ) r(n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R4VEC_UNIFORM - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop
  end if

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + 2147483647
    end if

    r(i) = a + ( b - a ) * real ( seed, kind = 4 ) * 4.656612875E-10

  end do

  return
end
subroutine r8_sct ( n, x, y )

!*****************************************************************************80
!
!! R8_SCT computes a double precision "slow" cosine transform.
!
!  Discussion:
!
!    This routine is provided for illustration and testing.  It is inefficient
!    relative to optimized routines that use fast Fourier techniques.
!
!      Y(1) = Sum ( 1 <= J <= N ) X(J)
!
!      For 2 <= I <= N-1:
!
!        Y(I) = 2 * Sum ( 1 <= J <= N ) X(J)
!          * cos ( PI * ( I - 1 ) * ( J - 1 ) / ( N - 1 ) )
!
!      Y(N) = Sum ( X(1:N:2) ) - Sum ( X(2:N:2) )
!
!    Applying the routine twice in succession should yield the original data,
!    multiplied by 2 * ( N + 1 ).  This is a good check for correctness
!    and accuracy.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 March 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of data values.
!
!    Input, real ( kind = 8 ) X(N), the data sequence.
!
!    Output, real ( kind = 8 ) Y(N), the transformed data.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 8 ) angle
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real    ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real    ( kind = 8 ) x(n)
  real    ( kind = 8 ) y(n)

  do i = 1, n

    y(i) = x(1) / 2.0D+00

    do j = 2, n - 1
      angle = pi &
        * real ( mod ( ( i - 1 ) * ( j - 1 ), 2 * ( n - 1 ) ), kind = 8 ) &
        / real ( n - 1, kind = 8 )
      y(i) = y(i) + x(j) * cos ( angle )
    end do

    j = n

    angle = pi &
      * real ( mod ( ( i - 1 ) * ( j - 1 ), 2 * ( n - 1 ) ), kind = 8 ) &
      / real ( n - 1, kind = 8 )

    y(i) = y(i) + x(n) * cos ( angle ) / 2.0D+00

  end do

  y(1:n) = 2.0D+00 * y(1:n) &
    * sqrt ( real ( n, kind = 8 ) / real ( n - 1, kind = 8 ) )

  return
end
subroutine r8_sct_old ( n, x, y )

!*****************************************************************************80
!
!! R8_SCT computes a double precision "slow" cosine transform.
!
!  Discussion:
!
!    This routine is provided for illustration and testing.  It is inefficient
!    relative to optimized routines that use fast Fourier techniques.
!
!      Y(1) = Sum ( 1 <= J <= N ) X(J)
!
!      For 2 <= I <= N-1:
!
!        Y(I) = 2 * Sum ( 1 <= J <= N ) X(J)
!          * cos ( PI * ( I - 1 ) * ( J - 1 ) / ( N - 1 ) )
!
!      Y(N) = Sum ( X(1:N:2) ) - Sum ( X(2:N:2) )
!
!    Applying the routine twice in succession should yield the original data,
!    multiplied by 2 * ( N + 1 ).  This is a good check for correctness
!    and accuracy.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 January 2002
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of data values.
!
!    Input, real ( kind = 8 ) X(N), the data sequence.
!
!    Output, real ( kind = 8 ) Y(N), the transformed data.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real    ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real    ( kind = 8 ) theta
  real    ( kind = 8 ) x(n)
  real    ( kind = 8 ) y(n)

  y(1) = sum ( x(1:n) )

  do i = 2, n-1
    y(i) = 0.0D+00
    do j = 1, n
      theta = pi * &
        real ( mod ( ( j - 1 ) * ( i - 1 ), 2 * ( n - 1 ) ), kind = 8 ) &
        / real ( n - 1, kind = 8 )
      y(i) = y(i) + 2.0D+00 * x(j) * cos ( theta )
    end do
  end do

  y(n) = sum ( x(1:n:2) ) - sum ( x(2:n:2) )

  return
end
subroutine r8_sst ( n, x, y )

!*****************************************************************************80
!
!! R8_SST computes a double precision "slow" sine transform.
!
!  Discussion:
!
!    This routine is provided for illustration and testing.  It is inefficient
!    relative to optimized routines that use fast Fourier techniques.
!
!    For 1 <= I <= N,
!
!      Y(I) = Sum ( 1 <= J <= N ) X(J) * sin ( PI * I * J / ( N + 1 ) )
!
!    Applying the routine twice in succession should yield the original data,
!    multiplied by N / 2.  This is a good check for correctness and accuracy.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 December 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of data values.
!
!    Input, real ( kind = 8 ) X(N), the data sequence.
!
!    Output, real ( kind = 8 ) Y(N), the transformed data.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  real    ( kind = 8 ), parameter :: pi = 3.141592653589793D+00
  real    ( kind = 8 ) theta(n)
  real    ( kind = 8 ) x(n)
  real    ( kind = 8 ) y(n)

  call r8vec_indicator ( n, theta )
  theta(1:n) = theta(1:n) * pi / real ( n + 1, kind = 8 )

  y(1:n) = 0.0D+00

  do i = 1, n
    y(1:n) = y(1:n) + 2.0D+00 * x(i) * sin ( real ( i, kind = 8 ) * theta(1:n) )
  end do

  return
end
function r8_uniform ( a, b, seed )

!*****************************************************************************80
!
!! R8_UNIFORM returns a scaled pseudorandom R8.
!
!  Discussion:
!
!    An R8 is a real ( kind = 8 ) value.
!
!    For now, the input quantity SEED is an integer ( kind = 4 ) variable.
!
!    The pseudorandom number should be uniformly distributed
!    between A and B.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the limits of the interval.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which should
!    NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R8_UNIFORM, a number strictly between A and B.
!
  implicit none

  real    ( kind = 8 ) a
  real    ( kind = 8 ) b
  integer ( kind = 4 ) k
  real    ( kind = 8 ) r8_uniform
  integer ( kind = 4 )seed

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8_UNIFORM - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop
  end if

  k = seed / 127773

  seed = 16807 * ( seed - k * 127773 ) - k * 2836

  if ( seed < 0 ) then
    seed = seed + 2147483647
  end if

  r8_uniform = a + ( b - a ) * real ( seed, kind = 8 ) * 4.656612875D-10

  return
end
subroutine r8vec_indicator ( n, a )

!*****************************************************************************80
!
!! R8VEC_INDICATOR sets an R8VEC to the indicator vector.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements of A.
!
!    Output, real ( kind = 8 ) A(N), the array to be initialized.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 8 ) a(n)
  integer ( kind = 4 ) i

  do i = 1, n
    a(i) = real ( i, kind = 8 )
  end do

  return
end
subroutine r8vec_print_some ( n, a, max_print, title )

!*****************************************************************************80
!
!! R8VEC_PRINT_SOME prints "some" of an R8VEC.
!
!  Discussion:
!
!    The user specifies MAX_PRINT, the maximum number of lines to print.
!
!    If N, the size of the vector, is no more than MAX_PRINT, then
!    the entire vector is printed, one entry per line.
!
!    Otherwise, if possible, the first MAX_PRINT-2 entries are printed,
!    followed by a line of periods suggesting an omission,
!    and the last entry.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 December 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries of the vector.
!
!    Input, real ( kind = 8 ) A(N), the vector to be printed.
!
!    Input, integer ( kind = 4 ) MAX_PRINT, the maximum number of lines to print.
!
!    Input, character ( len = * ) TITLE, an optional title.
!
  implicit none

  integer   ( kind = 4 ) n

  real      ( kind = 8 ) a(n)
  integer   ( kind = 4 ) i
  integer   ( kind = 4 ) max_print
  character ( len = * ) title

  if ( max_print <= 0 ) then
    return
  end if

  if ( n <= 0 ) then
    return
  end if

  if ( 0 < len_trim ( title ) ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) trim ( title )
    write ( *, '(a)' ) ' '
  end if

  if ( n <= max_print ) then

    do i = 1, n
      write ( *, '(i8,2x,g14.6)' ) i, a(i)
    end do

  else if ( 3 <= max_print ) then

    do i = 1, max_print-2
      write ( *, '(i8,2x,g14.6)' ) i, a(i)
    end do
    write ( *, '(a)' ) '......  ..............'
    i = n
    write ( *, '(i8,2x,g14.6)' ) i, a(i)

  else

    do i = 1, max_print - 1
      write ( *, '(i8,2x,g14.6)' ) i, a(i)
    end do
    i = max_print
    write ( *, '(i8,2x,g14.6,2x,a)' ) i, a(i), '...more entries...'

  end if

  return
end
subroutine r8vec_uniform ( n, a, b, seed, r )

!*****************************************************************************80
!
!! R8VEC_UNIFORM returns a scaled pseudorandom R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!    For now, the input quantity SEED is an integer ( kind = 4 ) variable.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) A, B, the lower and upper limits.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(N), the vector of pseudorandom values.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 8 ) a
  real    ( kind = 8 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real    ( kind = 8 ) r(n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_UNIFORM - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop
  end if

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + 2147483647
    end if

    r(i) = a + ( b - a ) * real ( seed, kind = 8 ) * 4.656612875D-10

  end do

  return
end
subroutine radb2 ( ido, l1, cc, ch, wa1 )

!*****************************************************************************80
!
!! RADB2 is a lower level routine used by RFFTB1.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) ido
  integer ( kind = 4 ) l1

  real    ( kind = 4 ) cc(ido,2,l1)
  real    ( kind = 4 ) ch(ido,l1,2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ic
  integer ( kind = 4 ) k
  real    ( kind = 4 ) ti2
  real    ( kind = 4 ) tr2
  real    ( kind = 4 ) wa1(ido)

  ch(1,1:l1,1) = cc(1,1,1:l1) + cc(ido,2,1:l1)
  ch(1,1:l1,2) = cc(1,1,1:l1) - cc(ido,2,1:l1)

  if ( ido < 2 ) then
    return
  end if

  if ( 2 < ido ) then

    do k = 1, l1
      do i = 3, ido, 2

        ic = ido + 2 - i

        ch(i-1,k,1) = cc(i-1,1,k) + cc(ic-1,2,k)
        tr2         = cc(i-1,1,k) - cc(ic-1,2,k)
        ch(i,k,1)   = cc(i,1,k)   - cc(ic,2,k)
        ti2         = cc(i,1,k)   + cc(ic,2,k)

        ch(i-1,k,2) = wa1(i-2) * tr2 - wa1(i-1) * ti2
        ch(i,k,2)   = wa1(i-2) * ti2 + wa1(i-1) * tr2

      end do
    end do

    if ( mod ( ido, 2 ) == 1 ) then
      return
    end if

  end if

  ch(ido,1:l1,1) =    cc(ido,1,1:l1) + cc(ido,1,1:l1)
  ch(ido,1:l1,2) = -( cc(1,2,1:l1)   + cc(1,2,1:l1) )

  return
end
subroutine radb3 ( ido, l1, cc, ch, wa1, wa2 )

!*****************************************************************************80
!
!! RADB3 is a lower level routine used by RFFTB1.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) ido
  integer ( kind = 4 ) l1

  real    ( kind = 4 ) cc(ido,3,l1)
  real    ( kind = 4 ) ch(ido,l1,3)
  real    ( kind = 4 ) ci2
  real    ( kind = 4 ) ci3
  real    ( kind = 4 ) cr2
  real    ( kind = 4 ) cr3
  real    ( kind = 4 ) di2
  real    ( kind = 4 ) di3
  real    ( kind = 4 ) dr2
  real    ( kind = 4 ) dr3
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ic
  integer ( kind = 4 ) k
  real    ( kind = 4 ) taui
  real    ( kind = 4 ), parameter :: taur = -0.5E+00
  real    ( kind = 4 ) ti2
  real    ( kind = 4 ) tr2
  real    ( kind = 4 ) wa1(ido)
  real    ( kind = 4 ) wa2(ido)

  taui = sqrt ( 3.0E+00 ) / 2.0E+00

  do k = 1, l1

    tr2 = cc(ido,2,k) + cc(ido,2,k)
    cr2 = cc(1,1,k) + taur * tr2
    ch(1,k,1) = cc(1,1,k) + tr2
    ci3 = taui * ( cc(1,3,k) + cc(1,3,k) )

    ch(1,k,2) = cr2 - ci3
    ch(1,k,3) = cr2 + ci3

  end do

  if ( ido == 1 ) then
    return
  end if

  do k = 1, l1
    do i = 3, ido, 2

      ic = ido + 2 - i

      tr2 = cc(i-1,3,k) + cc(ic-1,2,k)
      cr2 = cc(i-1,1,k) + taur * tr2
      ch(i-1,k,1) = cc(i-1,1,k) + tr2

      ti2 = cc(i,3,k) - cc(ic,2,k)
      ci2 = cc(i,1,k) + taur * ti2
      ch(i,k,1) = cc(i,1,k) + ti2

      cr3 = taui * ( cc(i-1,3,k) - cc(ic-1,2,k) )
      ci3 = taui * ( cc(i,3,k)   + cc(ic,2,k) )

      dr2 = cr2 - ci3
      dr3 = cr2 + ci3
      di2 = ci2 + cr3
      di3 = ci2 - cr3

      ch(i-1,k,2) = wa1(i-2) * dr2 - wa1(i-1) * di2
      ch(i,k,2)   = wa1(i-2) * di2 + wa1(i-1) * dr2
      ch(i-1,k,3) = wa2(i-2) * dr3 - wa2(i-1) * di3
      ch(i,k,3)   = wa2(i-2) * di3 + wa2(i-1) * dr3

    end do
  end do

  return
end
subroutine radb4 ( ido, l1, cc, ch, wa1, wa2, wa3 )

!*****************************************************************************80
!
!! RADB4 is a lower level routine used by RFFTB1.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) ido
  integer ( kind = 4 ) l1

  real    ( kind = 4 ) cc(ido,4,l1)
  real    ( kind = 4 ) ch(ido,l1,4)
  real    ( kind = 4 ) ci2
  real    ( kind = 4 ) ci3
  real    ( kind = 4 ) ci4
  real    ( kind = 4 ) cr2
  real    ( kind = 4 ) cr3
  real    ( kind = 4 ) cr4
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ic
  integer ( kind = 4 ) k
  real    ( kind = 4 ), parameter :: sqrt2 = 1.414213562373095E+00
  real    ( kind = 4 ) ti1
  real    ( kind = 4 ) ti2
  real    ( kind = 4 ) ti3
  real    ( kind = 4 ) ti4
  real    ( kind = 4 ) tr1
  real    ( kind = 4 ) tr2
  real    ( kind = 4 ) tr3
  real    ( kind = 4 ) tr4
  real    ( kind = 4 ) wa1(ido)
  real    ( kind = 4 ) wa2(ido)
  real    ( kind = 4 ) wa3(ido)

  do k = 1, l1

    tr1 = cc(1,1,k) - cc(ido,4,k)
    tr2 = cc(1,1,k) + cc(ido,4,k)
    tr3 = cc(ido,2,k) + cc(ido,2,k)
    tr4 = cc(1,3,k) + cc(1,3,k)

    ch(1,k,1) = tr2 + tr3
    ch(1,k,2) = tr1 - tr4
    ch(1,k,3) = tr2 - tr3
    ch(1,k,4) = tr1 + tr4

  end do

  if ( ido < 2 ) then
    return
  end if

  if ( 2 < ido ) then

    do k = 1, l1
      do i = 3, ido, 2

        ic = ido + 2 - i

        ti1 = cc(i,1,k) + cc(ic,4,k)
        ti2 = cc(i,1,k) - cc(ic,4,k)
        ti3 = cc(i,3,k) - cc(ic,2,k)
        tr4 = cc(i,3,k) + cc(ic,2,k)

        tr1 = cc(i-1,1,k) - cc(ic-1,4,k)
        tr2 = cc(i-1,1,k) + cc(ic-1,4,k)
        ti4 = cc(i-1,3,k) - cc(ic-1,2,k)
        tr3 = cc(i-1,3,k) + cc(ic-1,2,k)

        ch(i-1,k,1) = tr2 + tr3
        cr3         = tr2 - tr3
        ch(i,k,1)   = ti2 + ti3
        ci3         = ti2 - ti3

        cr2 = tr1 - tr4
        cr4 = tr1 + tr4
        ci2 = ti1 + ti4
        ci4 = ti1 - ti4

        ch(i-1,k,2) = wa1(i-2) * cr2 - wa1(i-1) * ci2
        ch(i,k,2)   = wa1(i-2) * ci2 + wa1(i-1) * cr2
        ch(i-1,k,3) = wa2(i-2) * cr3 - wa2(i-1) * ci3
        ch(i,k,3)   = wa2(i-2) * ci3 + wa2(i-1) * cr3
        ch(i-1,k,4) = wa3(i-2) * cr4 - wa3(i-1) * ci4
        ch(i,k,4)   = wa3(i-2) * ci4 + wa3(i-1) * cr4

      end do
    end do

    if ( mod ( ido, 2 ) == 1 ) then
      return
    end if

  end if

  do k = 1, l1

    ti1 = cc(1,2,k)   + cc(1,4,k)
    ti2 = cc(1,4,k)   - cc(1,2,k)
    tr1 = cc(ido,1,k) - cc(ido,3,k)
    tr2 = cc(ido,1,k) + cc(ido,3,k)

    ch(ido,k,1) = tr2 + tr2
    ch(ido,k,2) = sqrt2 * ( tr1 - ti1 )
    ch(ido,k,3) = ti2 + ti2
    ch(ido,k,4) = -sqrt2 * ( tr1 + ti1 )

  end do

  return
end
subroutine radb5 ( ido, l1, cc, ch, wa1, wa2, wa3, wa4 )

!*****************************************************************************80
!
!! RADB5 is a lower level routine used by RFFTB1.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) ido
  integer ( kind = 4 ) l1

  real    ( kind = 4 ) cc(ido,5,l1)
  real    ( kind = 4 ) ch(ido,l1,5)
  real    ( kind = 4 ) ci2
  real    ( kind = 4 ) ci3
  real    ( kind = 4 ) ci4
  real    ( kind = 4 ) ci5
  real    ( kind = 4 ) cr2
  real    ( kind = 4 ) cr3
  real    ( kind = 4 ) cr4
  real    ( kind = 4 ) cr5
  real    ( kind = 4 ) di2
  real    ( kind = 4 ) di3
  real    ( kind = 4 ) di4
  real    ( kind = 4 ) di5
  real    ( kind = 4 ) dr2
  real    ( kind = 4 ) dr3
  real    ( kind = 4 ) dr4
  real    ( kind = 4 ) dr5
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ic
  integer ( kind = 4 ) k
  real    ( kind = 4 ), parameter :: ti11 =  0.951056516295154E+00
  real    ( kind = 4 ), parameter :: ti12 =  0.587785252292473E+00
  real    ( kind = 4 ) ti2
  real    ( kind = 4 ) ti3
  real    ( kind = 4 ) ti4
  real    ( kind = 4 ) ti5
  real    ( kind = 4 ), parameter :: tr11 =  0.309016994374947E+00
  real    ( kind = 4 ), parameter :: tr12 = -0.809016994374947E+00
  real    ( kind = 4 ) tr2
  real    ( kind = 4 ) tr3
  real    ( kind = 4 ) tr4
  real    ( kind = 4 ) tr5
  real    ( kind = 4 ) wa1(ido)
  real    ( kind = 4 ) wa2(ido)
  real    ( kind = 4 ) wa3(ido)
  real    ( kind = 4 ) wa4(ido)

  do k = 1, l1

    ti5 = cc(1,3,k) + cc(1,3,k)
    ti4 = cc(1,5,k) + cc(1,5,k)
    tr2 = cc(ido,2,k) + cc(ido,2,k)
    tr3 = cc(ido,4,k) + cc(ido,4,k)

    ch(1,k,1) = cc(1,1,k) + tr2 + tr3
    cr2 = cc(1,1,k) + tr11 * tr2 + tr12 * tr3
    cr3 = cc(1,1,k) + tr12 * tr2 + tr11 * tr3
    ci5 = ti11 * ti5 + ti12 * ti4
    ci4 = ti12 * ti5 - ti11 * ti4

    ch(1,k,2) = cr2 - ci5
    ch(1,k,3) = cr3 - ci4
    ch(1,k,4) = cr3 + ci4
    ch(1,k,5) = cr2 + ci5

  end do

  if ( ido == 1 ) then
    return
  end if

  do k = 1, l1
    do i = 3, ido, 2

      ic = ido + 2 - i

      ti5 = cc(i,3,k) + cc(ic,2,k)
      ti2 = cc(i,3,k) - cc(ic,2,k)
      ti4 = cc(i,5,k) + cc(ic,4,k)
      ti3 = cc(i,5,k) - cc(ic,4,k)
      tr5 = cc(i-1,3,k) - cc(ic-1,2,k)
      tr2 = cc(i-1,3,k) + cc(ic-1,2,k)
      tr4 = cc(i-1,5,k) - cc(ic-1,4,k)
      tr3 = cc(i-1,5,k) + cc(ic-1,4,k)

      ch(i-1,k,1) = cc(i-1,1,k) + tr2 + tr3
      ch(i,k,1)   = cc(i,1,k) + ti2 + ti3

      cr2 = cc(i-1,1,k) + tr11 * tr2 + tr12 * tr3
      ci2 = cc(i,1,k)   + tr11 * ti2 + tr12 * ti3
      cr3 = cc(i-1,1,k) + tr12 * tr2 + tr11 * tr3
      ci3 = cc(i,1,k)   + tr12 * ti2 + tr11 * ti3

      cr5 = ti11 * tr5 + ti12 * tr4
      ci5 = ti11 * ti5 + ti12 * ti4
      cr4 = ti12 * tr5 - ti11 * tr4
      ci4 = ti12 * ti5 - ti11 * ti4

      dr3 = cr3 - ci4
      dr4 = cr3 + ci4
      di3 = ci3 + cr4
      di4 = ci3 - cr4
      dr5 = cr2 + ci5
      dr2 = cr2 - ci5
      di5 = ci2 - cr5
      di2 = ci2 + cr5

      ch(i-1,k,2) = wa1(i-2) * dr2 - wa1(i-1) * di2
      ch(i,k,2)   = wa1(i-2) * di2 + wa1(i-1) * dr2
      ch(i-1,k,3) = wa2(i-2) * dr3 - wa2(i-1) * di3
      ch(i,k,3)   = wa2(i-2) * di3 + wa2(i-1) * dr3
      ch(i-1,k,4) = wa3(i-2) * dr4 - wa3(i-1) * di4
      ch(i,k,4)   = wa3(i-2) * di4 + wa3(i-1) * dr4
      ch(i-1,k,5) = wa4(i-2) * dr5 - wa4(i-1) * di5
      ch(i,k,5)   = wa4(i-2) * di5 + wa4(i-1) * dr5

    end do
  end do

  return
end
subroutine radbg ( ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa )

!*****************************************************************************80
!
!! RADBG is a lower level routine used by RFFTB1.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) idl1
  integer ( kind = 4 ) ido
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) l1

  real    ( kind = 4 ) ai1
  real    ( kind = 4 ) ai2
  real    ( kind = 4 ) ar1
  real    ( kind = 4 ) ar1h
  real    ( kind = 4 ) ar2
  real    ( kind = 4 ) ar2h
  real    ( kind = 4 ) arg
  real    ( kind = 4 ) c1(ido,l1,ip)
  real    ( kind = 4 ) c2(idl1,ip)
  real    ( kind = 4 ) cc(ido,ip,l1)
  real    ( kind = 4 ) ch(ido,l1,ip)
  real    ( kind = 4 ) ch2(idl1,ip)
  real    ( kind = 4 ) dc2
  real    ( kind = 4 ) dcp
  real    ( kind = 4 ) ds2
  real    ( kind = 4 ) dsp
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ic
  integer ( kind = 4 ) idij
  integer ( kind = 4 ) ik
  integer ( kind = 4 ) ipph
  integer ( kind = 4 ) is
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) jc
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) lc
  integer ( kind = 4 ) nbd
  real    ( kind = 4 ), parameter :: pi = 3.1415926E+00
  real    ( kind = 4 ) wa(*)

  arg = 2.0E+00 * pi / real ( ip, kind = 4 )
  dcp = cos ( arg )
  dsp = sin ( arg )
  nbd = ( ido - 1 ) / 2
  ipph = ( ip + 1 ) / 2
  ch(1:ido,1:l1,1) = cc(1:ido,1,1:l1)

  do j = 2, ipph
    jc = ip + 2 - j
    j2 = j + j
    ch(1,1:l1,j) =  cc(ido,j2-2,1:l1) + cc(ido,j2-2,1:l1)
    ch(1,1:l1,jc) = cc(1,j2-1,1:l1)   + cc(1,j2-1,1:l1)
  end do

  if ( ido /= 1 ) then

    if ( l1 <= nbd ) then

      do j = 2, ipph
        jc = ip + 2 - j
        do k = 1, l1
          do i = 3, ido, 2
            ic = ido + 2 - i
            ch(i-1,k,j)  = cc(i-1,2*j-1,k) + cc(ic-1,2*j-2,k)
            ch(i-1,k,jc) = cc(i-1,2*j-1,k) - cc(ic-1,2*j-2,k)
            ch(i,k,j)    = cc(i,2*j-1,k)   - cc(ic,2*j-2,k)
            ch(i,k,jc)   = cc(i,2*j-1,k)   + cc(ic,2*j-2,k)
          end do
        end do
      end do

    else

      do j = 2, ipph
        jc = ip + 2 - j
        do i = 3, ido, 2
          ic = ido + 2 - i
          ch(i-1,1:l1,j)  = cc(i-1,2*j-1,1:l1) + cc(ic-1,2*j-2,1:l1)
          ch(i-1,1:l1,jc) = cc(i-1,2*j-1,1:l1) - cc(ic-1,2*j-2,1:l1)
          ch(i,1:l1,j)    = cc(i,2*j-1,1:l1)   - cc(ic,2*j-2,1:l1)
          ch(i,1:l1,jc)   = cc(i,2*j-1,1:l1)   + cc(ic,2*j-2,1:l1)
        end do
      end do

    end if

  end if

  ar1 = 1.0E+00
  ai1 = 0.0E+00

  do l = 2, ipph

    lc = ip + 2 - l
    ar1h = dcp * ar1 - dsp * ai1
    ai1 =  dcp * ai1 + dsp * ar1
    ar1 = ar1h

    do ik = 1, idl1
      c2(ik,l)  = ch2(ik,1) + ar1 * ch2(ik,2)
      c2(ik,lc) =             ai1 * ch2(ik,ip)
    end do

    dc2 = ar1
    ds2 = ai1
    ar2 = ar1
    ai2 = ai1

    do j = 3, ipph

      jc = ip + 2 - j
      ar2h = dc2 * ar2 - ds2 * ai2
      ai2  = dc2 * ai2 + ds2 * ar2
      ar2 = ar2h

      do ik = 1, idl1
        c2(ik,l)  = c2(ik,l)  + ar2 * ch2(ik,j)
        c2(ik,lc) = c2(ik,lc) + ai2 * ch2(ik,jc)
      end do

    end do

  end do

  do j = 2, ipph
    ch2(1:idl1,1) = ch2(1:idl1,1) + ch2(1:idl1,j)
  end do

  do j = 2, ipph
    jc = ip + 2 - j
    ch(1,1:l1,j)  = c1(1,1:l1,j) - c1(1,1:l1,jc)
    ch(1,1:l1,jc) = c1(1,1:l1,j) + c1(1,1:l1,jc)
  end do

  if ( ido /= 1 ) then

    if ( l1 <= nbd ) then

      do j = 2, ipph
        jc = ip + 2 - j
        do k = 1, l1
          do i = 3, ido, 2
            ch(i-1,k,j)  = c1(i-1,k,j) - c1(i,k,jc)
            ch(i-1,k,jc) = c1(i-1,k,j) + c1(i,k,jc)
            ch(i,k,j)    = c1(i,k,j)   + c1(i-1,k,jc)
            ch(i,k,jc)   = c1(i,k,j)   - c1(i-1,k,jc)
          end do
        end do
      end do

    else

      do j = 2, ipph
        jc = ip + 2 - j
        do i = 3, ido, 2
          ch(i-1,1:l1,j)  = c1(i-1,1:l1,j) - c1(i,1:l1,jc)
          ch(i-1,1:l1,jc) = c1(i-1,1:l1,j) + c1(i,1:l1,jc)
          ch(i,1:l1,j)    = c1(i,1:l1,j)   + c1(i-1,1:l1,jc)
          ch(i,1:l1,jc)   = c1(i,1:l1,j)   - c1(i-1,1:l1,jc)
        end do
      end do

    end if

  end if

  if ( ido == 1 ) then
    return
  end if

  c2(1:idl1,1) = ch2(1:idl1,1)
  c1(1,1:l1,2:ip) = ch(1,1:l1,2:ip)

  if ( nbd <= l1 ) then

    is = -ido

    do j = 2, ip
      is = is + ido
      idij = is
      do i = 3, ido, 2
        idij = idij + 2
        c1(i-1,1:l1,j) = wa(idij-1) * ch(i-1,1:l1,j) - wa(idij) * ch(i,1:l1,j)
        c1(i,1:l1,j)   = wa(idij-1) * ch(i,1:l1,j)   + wa(idij) * ch(i-1,1:l1,j)
      end do
    end do

  else

    is = -ido
    do j = 2, ip
      is = is + ido
      do k = 1, l1
        idij = is
        do i = 3, ido, 2
          idij = idij + 2
          c1(i-1,k,j) = wa(idij-1) * ch(i-1,k,j) - wa(idij) * ch(i,k,j)
          c1(i,k,j)   = wa(idij-1) * ch(i,k,j)   + wa(idij) * ch(i-1,k,j)
        end do
      end do
    end do

  end if

  return
end
subroutine radf2 ( ido, l1, cc, ch, wa1 )

!*****************************************************************************80
!
!! RADF2 is a lower level routine used by RFFTF1.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) ido
  integer ( kind = 4 ) l1

  real    ( kind = 4 ) cc(ido,l1,2)
  real    ( kind = 4 ) ch(ido,2,l1)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ic
  integer ( kind = 4 ) k
  real    ( kind = 4 ) ti2
  real    ( kind = 4 ) tr2
  real    ( kind = 4 ) wa1(ido)

  ch(1,1,1:l1)   = cc(1,1:l1,1) + cc(1,1:l1,2)
  ch(ido,2,1:l1) = cc(1,1:l1,1) - cc(1,1:l1,2)

  if ( ido < 2 ) then
    return
  end if

  if ( 2 < ido ) then

    do k = 1, l1
      do i = 3, ido, 2

        ic = ido + 2 - i

        tr2 = wa1(i-2) * cc(i-1,k,2) + wa1(i-1) * cc(i,k,2)
        ti2 = wa1(i-2) * cc(i,k,2)   - wa1(i-1) * cc(i-1,k,2)

        ch(i,1,k) = cc(i,k,1) + ti2
        ch(ic,2,k) = ti2 - cc(i,k,1)
        ch(i-1,1,k) = cc(i-1,k,1) + tr2
        ch(ic-1,2,k) = cc(i-1,k,1) - tr2

      end do
    end do

    if ( mod ( ido, 2 ) == 1 ) then
      return
    end if

  end if

  ch(1,2,1:l1) = -cc(ido,1:l1,2)
  ch(ido,1,1:l1) = cc(ido,1:l1,1)

  return
end
subroutine radf3 ( ido, l1, cc, ch, wa1, wa2 )

!*****************************************************************************80
!
!! RADF3 is a lower level routine used by RFFTF1.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) ido
  integer ( kind = 4 ) l1

  real    ( kind = 4 ) cc(ido,l1,3)
  real    ( kind = 4 ) ch(ido,3,l1)
  real    ( kind = 4 ) ci2
  real    ( kind = 4 ) cr2
  real    ( kind = 4 ) di2
  real    ( kind = 4 ) di3
  real    ( kind = 4 ) dr2
  real    ( kind = 4 ) dr3
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ic
  integer ( kind = 4 ) k
  real    ( kind = 4 ) taui
  real    ( kind = 4 ), parameter :: taur = -0.5E+00
  real    ( kind = 4 ) ti2
  real    ( kind = 4 ) ti3
  real    ( kind = 4 ) tr2
  real    ( kind = 4 ) tr3
  real    ( kind = 4 ) wa1(ido)
  real    ( kind = 4 ) wa2(ido)

  taui = sqrt ( 3.0E+00 ) / 2.0E+00

  do k = 1, l1
    cr2 = cc(1,k,2) + cc(1,k,3)
    ch(1,1,k) = cc(1,k,1) + cr2
    ch(1,3,k) = taui * ( cc(1,k,3) - cc(1,k,2) )
    ch(ido,2,k) = cc(1,k,1) + taur * cr2
  end do

  if ( ido == 1 ) then
    return
  end if

  do k = 1, l1
    do i = 3, ido, 2

      ic = ido + 2 - i

      dr2 = wa1(i-2) * cc(i-1,k,2) + wa1(i-1) * cc(i,k,2)
      di2 = wa1(i-2) * cc(i,k,2)   - wa1(i-1) * cc(i-1,k,2)
      dr3 = wa2(i-2) * cc(i-1,k,3) + wa2(i-1) * cc(i,k,3)
      di3 = wa2(i-2) * cc(i,k,3)   - wa2(i-1) * cc(i-1,k,3)

      cr2 = dr2 + dr3
      ci2 = di2 + di3

      ch(i-1,1,k) = cc(i-1,k,1) + cr2
      ch(i,1,k)   = cc(i,k,1) + ci2

      tr2 = cc(i-1,k,1) + taur * cr2
      ti2 = cc(i,k,1) + taur * ci2
      tr3 = taui * ( di2 - di3 )
      ti3 = taui * ( dr3 - dr2 )

      ch(i-1,3,k) = tr2 + tr3
      ch(ic-1,2,k) = tr2 - tr3
      ch(i,3,k) = ti2 + ti3
      ch(ic,2,k) = ti3 - ti2

    end do
  end do

  return
end
subroutine radf4 ( ido, l1, cc, ch, wa1, wa2, wa3 )

!*****************************************************************************80
!
!! RADF4 is a lower level routine used by RFFTF1.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) ido
  integer ( kind = 4 ) l1

  real    ( kind = 4 ) cc(ido,l1,4)
  real    ( kind = 4 ) ch(ido,4,l1)
  real    ( kind = 4 ) ci2
  real    ( kind = 4 ) ci3
  real    ( kind = 4 ) ci4
  real    ( kind = 4 ) cr2
  real    ( kind = 4 ) cr3
  real    ( kind = 4 ) cr4
  real    ( kind = 4 ) hsqt2
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ic
  integer ( kind = 4 ) k
  real    ( kind = 4 ) ti1
  real    ( kind = 4 ) ti2
  real    ( kind = 4 ) ti3
  real    ( kind = 4 ) ti4
  real    ( kind = 4 ) tr1
  real    ( kind = 4 ) tr2
  real    ( kind = 4 ) tr3
  real    ( kind = 4 ) tr4
  real    ( kind = 4 ) wa1(ido)
  real    ( kind = 4 ) wa2(ido)
  real    ( kind = 4 ) wa3(ido)

  hsqt2 = sqrt ( 2.0E+00 ) / 2.0E+00

  do k = 1, l1
    tr1 = cc(1,k,2) + cc(1,k,4)
    tr2 = cc(1,k,1) + cc(1,k,3)
    ch(1,1,k) = tr1 + tr2
    ch(ido,4,k) = tr2 - tr1
    ch(ido,2,k) = cc(1,k,1) - cc(1,k,3)
    ch(1,3,k) = cc(1,k,4) - cc(1,k,2)
  end do

  if ( ido < 2 ) then
    return
  end if

  if ( 2 < ido ) then

    do k = 1, l1
      do i = 3, ido, 2

        ic = ido + 2 - i

        cr2 = wa1(i-2) * cc(i-1,k,2) + wa1(i-1) * cc(i,k,2)
        ci2 = wa1(i-2) * cc(i,k,2)   - wa1(i-1) * cc(i-1,k,2)
        cr3 = wa2(i-2) * cc(i-1,k,3) + wa2(i-1) * cc(i,k,3)
        ci3 = wa2(i-2) * cc(i,k,3)   - wa2(i-1) * cc(i-1,k,3)
        cr4 = wa3(i-2) * cc(i-1,k,4) + wa3(i-1) * cc(i,k,4)
        ci4 = wa3(i-2) * cc(i,k,4)   - wa3(i-1) * cc(i-1,k,4)

        tr1 = cr2 + cr4
        tr4 = cr4 - cr2
        ti1 = ci2 + ci4
        ti4 = ci2 - ci4
        ti2 = cc(i,k,1) + ci3
        ti3 = cc(i,k,1) - ci3
        tr2 = cc(i-1,k,1) + cr3
        tr3 = cc(i-1,k,1) - cr3

        ch(i-1,1,k)  = tr1 + tr2
        ch(ic-1,4,k) = tr2 - tr1
        ch(i,1,k)    = ti1 + ti2
        ch(ic,4,k)   = ti1 - ti2
        ch(i-1,3,k)  = ti4 + tr3
        ch(ic-1,2,k) = tr3 - ti4
        ch(i,3,k)    = tr4 + ti3
        ch(ic,2,k)   = tr4 - ti3

      end do
    end do

    if ( mod ( ido, 2 ) == 1 ) then
      return
    end if

  end if

  do k = 1, l1

    ti1 = -hsqt2 * ( cc(ido,k,2) + cc(ido,k,4) )
    tr1 =  hsqt2 * ( cc(ido,k,2) - cc(ido,k,4) )

    ch(ido,1,k) = tr1 + cc(ido,k,1)
    ch(ido,3,k) = cc(ido,k,1) - tr1

    ch(1,2,k) = ti1 - cc(ido,k,3)
    ch(1,4,k) = ti1 + cc(ido,k,3)

  end do

  return
end
subroutine radf5 ( ido, l1, cc, ch, wa1, wa2, wa3, wa4 )

!*****************************************************************************80
!
!! RADF5 is a lower level routine used by RFFTF1.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
  implicit none

  integer ( kind = 4 ) ido
  integer ( kind = 4 ) l1

  real    ( kind = 4 ) cc(ido,l1,5)
  real    ( kind = 4 ) ch(ido,5,l1)
  real    ( kind = 4 ) ci2
  real    ( kind = 4 ) ci3
  real    ( kind = 4 ) ci4
  real    ( kind = 4 ) ci5
  real    ( kind = 4 ) cr2
  real    ( kind = 4 ) cr3
  real    ( kind = 4 ) cr4
  real    ( kind = 4 ) cr5
  real    ( kind = 4 ) di2
  real    ( kind = 4 ) di3
  real    ( kind = 4 ) di4
  real    ( kind = 4 ) di5
  real    ( kind = 4 ) dr2
  real    ( kind = 4 ) dr3
  real    ( kind = 4 ) dr4
  real    ( kind = 4 ) dr5
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ic
  integer ( kind = 4 ) k
  real    ( kind = 4 ), parameter :: ti11 =  0.951056516295154E+00
  real    ( kind = 4 ), parameter :: ti12 =  0.587785252292473E+00
  real    ( kind = 4 ) ti2
  real    ( kind = 4 ) ti3
  real    ( kind = 4 ) ti4
  real    ( kind = 4 ) ti5
  real    ( kind = 4 ), parameter :: tr11 =  0.309016994374947E+00
  real    ( kind = 4 ), parameter :: tr12 = -0.809016994374947E+00
  real    ( kind = 4 ) tr2
  real    ( kind = 4 ) tr3
  real    ( kind = 4 ) tr4
  real    ( kind = 4 ) tr5
  real    ( kind = 4 ) wa1(ido)
  real    ( kind = 4 ) wa2(ido)
  real    ( kind = 4 ) wa3(ido)
  real    ( kind = 4 ) wa4(ido)

  do k = 1, l1

    cr2 = cc(1,k,5) + cc(1,k,2)
    ci5 = cc(1,k,5) - cc(1,k,2)
    cr3 = cc(1,k,4) + cc(1,k,3)
    ci4 = cc(1,k,4) - cc(1,k,3)

    ch(1,1,k)   = cc(1,k,1) + cr2 + cr3
    ch(ido,2,k) = cc(1,k,1) + tr11 * cr2 + tr12 * cr3
    ch(1,3,k)   = ti11 * ci5 + ti12 * ci4
    ch(ido,4,k) = cc(1,k,1) + tr12 * cr2 + tr11 * cr3
    ch(1,5,k)   = ti12 * ci5 - ti11 * ci4

  end do

  if ( ido == 1 ) then
    return
  end if

  do k = 1, l1
    do i = 3, ido, 2

      ic = ido + 2 - i

      dr2 = wa1(i-2) * cc(i-1,k,2) + wa1(i-1) * cc(i,k,2)
      di2 = wa1(i-2) * cc(i,k,2)   - wa1(i-1) * cc(i-1,k,2)
      dr3 = wa2(i-2) * cc(i-1,k,3) + wa2(i-1) * cc(i,k,3)
      di3 = wa2(i-2) * cc(i,k,3)   - wa2(i-1) * cc(i-1,k,3)
      dr4 = wa3(i-2) * cc(i-1,k,4) + wa3(i-1) * cc(i,k,4)
      di4 = wa3(i-2) * cc(i,k,4)   - wa3(i-1) * cc(i-1,k,4)
      dr5 = wa4(i-2) * cc(i-1,k,5) + wa4(i-1) * cc(i,k,5)
      di5 = wa4(i-2) * cc(i,k,5)   - wa4(i-1) * cc(i-1,k,5)

      cr2 = dr2 + dr5
      ci5 = dr5 - dr2
      cr5 = di2 - di5
      ci2 = di2 + di5
      cr3 = dr3 + dr4
      ci4 = dr4 - dr3
      cr4 = di3 - di4
      ci3 = di3 + di4

      ch(i-1,1,k) = cc(i-1,k,1) + cr2 + cr3
      ch(i,1,k)   = cc(i,k,1)   + ci2 + ci3

      tr2 = cc(i-1,k,1) + tr11 * cr2 + tr12 * cr3
      ti2 = cc(i,k,1)   + tr11 * ci2 + tr12 * ci3
      tr3 = cc(i-1,k,1) + tr12 * cr2 + tr11 * cr3
      ti3 = cc(i,k,1)   + tr12 * ci2 + tr11 * ci3

      tr5 = ti11 * cr5 + ti12 * cr4
      ti5 = ti11 * ci5 + ti12 * ci4
      tr4 = ti12 * cr5 - ti11 * cr4
      ti4 = ti12 * ci5 - ti11 * ci4

      ch(i-1,3,k)  = tr2 + tr5
      ch(ic-1,2,k) = tr2 - tr5
      ch(i,3,k)    = ti2 + ti5
      ch(ic,2,k)   = ti5 - ti2
      ch(i-1,5,k)  = tr3 + tr4
      ch(ic-1,4,k) = tr3 - tr4
      ch(i,5,k)    = ti3 + ti4
      ch(ic,4,k)   = ti4 - ti3

    end do
  end do

  return
end
subroutine radfg ( ido, ip, l1, idl1, cc, c1, c2, ch, ch2, wa )

!*****************************************************************************80
!
!! RADFG is a lower level routine used by RFFTF1.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) IDO, ?
!
!    Input, integer ( kind = 4 ) IP, ?
!
!    Input, integer ( kind = 4 ) L1, ?
!
!    Input, integer ( kind = 4 ) IDL1, ?
!
!    ?, real ( kind = 4 ) CC(IDO,IP,L1), ?
!
!    ?, real ( kind = 4 ) C1(IDO,L1,IP), ?
!
!    ?, real ( kind = 4 ) C2(IDL1,IP), ?
!
!    ?, real ( kind = 4 ) CH(IDO,L1,IP), ?
!
!    ?, real ( kind = 4 ) CH2(IDL1,IP), ?
!
!    ?, real ( kind = 4 ) WA(*), ?
!
  implicit none

  integer ( kind = 4 ) idl1
  integer ( kind = 4 ) ido
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) l1

  real    ( kind = 4 ) ai1
  real    ( kind = 4 ) ai2
  real    ( kind = 4 ) ar1
  real    ( kind = 4 ) ar1h
  real    ( kind = 4 ) ar2
  real    ( kind = 4 ) ar2h
  real    ( kind = 4 ) arg
  real    ( kind = 4 ) c1(ido,l1,ip)
  real    ( kind = 4 ) c2(idl1,ip)
  real    ( kind = 4 ) cc(ido,ip,l1)
  real    ( kind = 4 ) ch(ido,l1,ip)
  real    ( kind = 4 ) ch2(idl1,ip)
  real    ( kind = 4 ) dc2
  real    ( kind = 4 ) dcp
  real    ( kind = 4 ) ds2
  real    ( kind = 4 ) dsp
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ic
  integer ( kind = 4 ) idij
  integer ( kind = 4 ) ik
  integer ( kind = 4 ) ipph
  integer ( kind = 4 ) is
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j2
  integer ( kind = 4 ) jc
  integer ( kind = 4 ) k
  integer ( kind = 4 ) l
  integer ( kind = 4 ) lc
  integer ( kind = 4 ) nbd
  real    ( kind = 4 ), parameter :: pi = 3.1415926E+00
  real    ( kind = 4 ) wa(*)

  arg = 2.0E+00 * pi / real ( ip, kind = 4 )
  dcp = cos ( arg )
  dsp = sin ( arg )
  ipph = ( ip + 1 ) / 2
  nbd = ( ido - 1 ) / 2

  if ( ido == 1 ) then

    c2(1:idl1,1) = ch2(1:idl1,1)

  else

    ch2(1:idl1,1) = c2(1:idl1,1)
    ch(1,1:l1,2:ip) = c1(1,1:l1,2:ip)

    if ( nbd <= l1 ) then

      is = -ido
      do j = 2, ip
        is = is + ido
        idij = is
        do i = 3, ido, 2
          idij = idij + 2
          do k = 1, l1
            ch(i-1,k,j) = wa(idij-1) * c1(i-1,k,j) + wa(idij) * c1(i,k,j)
            ch(i,k,j)   = wa(idij-1) * c1(i,k,j)   - wa(idij) * c1(i-1,k,j)
          end do
        end do
      end do

    else

      is = -ido

      do j = 2, ip
        is = is + ido
        do k = 1, l1
          idij = is
          do i = 3, ido, 2
            idij = idij + 2
            ch(i-1,k,j) = wa(idij-1) * c1(i-1,k,j) + wa(idij) * c1(i,k,j)
            ch(i,k,j)   = wa(idij-1) * c1(i,k,j)   - wa(idij) * c1(i-1,k,j)
          end do
        end do
      end do

    end if

    if ( l1 <= nbd ) then

      do j = 2, ipph
        jc = ip + 2 - j
        do k = 1, l1
          do i = 3, ido, 2
            c1(i-1,k,j)  = ch(i-1,k,j)  + ch(i-1,k,jc)
            c1(i-1,k,jc) = ch(i,k,j)    - ch(i,k,jc)
            c1(i,k,j)    = ch(i,k,j)    + ch(i,k,jc)
            c1(i,k,jc)   = ch(i-1,k,jc) - ch(i-1,k,j)
          end do
        end do
      end do

    else

      do j = 2, ipph
        jc = ip + 2 - j
        do i = 3, ido, 2
          c1(i-1,1:l1,j)  = ch(i-1,1:l1,j)  + ch(i-1,1:l1,jc)
          c1(i-1,1:l1,jc) = ch(i,1:l1,j)    - ch(i,1:l1,jc)
          c1(i,1:l1,j)    = ch(i,1:l1,j)    + ch(i,1:l1,jc)
          c1(i,1:l1,jc)   = ch(i-1,1:l1,jc) - ch(i-1,1:l1,j)
        end do
      end do

    end if

  end if

  do j = 2, ipph
    jc = ip + 2 - j
    c1(1,1:l1,j)  = ch(1,1:l1,j)  + ch(1,1:l1,jc)
    c1(1,1:l1,jc) = ch(1,1:l1,jc) - ch(1,1:l1,j)
  end do

  ar1 = 1.0E+00
  ai1 = 0.0E+00

  do l = 2, ipph

    lc = ip + 2 - l
    ar1h = dcp * ar1 - dsp * ai1
    ai1 =  dcp * ai1 + dsp * ar1
    ar1 = ar1h

    do ik = 1, idl1
      ch2(ik,l) = c2(ik,1) + ar1 * c2(ik,2)
      ch2(ik,lc) =           ai1 * c2(ik,ip)
    end do

    dc2 = ar1
    ds2 = ai1
    ar2 = ar1
    ai2 = ai1

    do j = 3, ipph

      jc = ip + 2 - j
      ar2h = dc2 * ar2 - ds2 * ai2
      ai2 =  dc2 * ai2 + ds2 * ar2
      ar2 = ar2h

      do ik = 1, idl1
        ch2(ik,l) =  ch2(ik,l)  + ar2 * c2(ik,j)
        ch2(ik,lc) = ch2(ik,lc) + ai2 * c2(ik,jc)
      end do

    end do

  end do

  do j = 2, ipph
    ch2(1:idl1,1) = ch2(1:idl1,1) + c2(1:idl1,j)
  end do

  cc(1:ido,1,1:l1) = ch(1:ido,1:l1,1)

  do j = 2, ipph
    jc = ip + 2 - j
    j2 = j + j
    cc(ido,j2-2,1:l1) = ch(1,1:l1,j)
    cc(1,j2-1,1:l1)   = ch(1,1:l1,jc)
  end do

  if ( ido == 1 ) then
    return
  end if

  if ( l1 <= nbd ) then

    do j = 2, ipph
      jc = ip + 2 - j
      j2 = j + j
      do k = 1, l1
        do i = 3, ido, 2
          ic = ido + 2 - i
          cc(i-1,j2-1,k)  = ch(i-1,k,j) + ch(i-1,k,jc)
          cc(ic-1,j2-2,k) = ch(i-1,k,j) - ch(i-1,k,jc)
          cc(i,j2-1,k)    = ch(i,k,j)   + ch(i,k,jc)
          cc(ic,j2-2,k)   = ch(i,k,jc)  - ch(i,k,j)
        end do
      end do
    end do

  else

    do j = 2, ipph
      jc = ip + 2 - j
      j2 = j + j
      do i = 3, ido, 2
        ic = ido + 2 - i
        cc(i-1,j2-1,1:l1)  = ch(i-1,1:l1,j) + ch(i-1,1:l1,jc)
        cc(ic-1,j2-2,1:l1) = ch(i-1,1:l1,j) - ch(i-1,1:l1,jc)
        cc(i,j2-1,1:l1)    = ch(i,1:l1,j)   + ch(i,1:l1,jc)
        cc(ic,j2-2,1:l1)   = ch(i,1:l1,jc)  - ch(i,1:l1,j)
      end do
    end do

  end if

  return
end
subroutine random_initialize ( seed )

!*****************************************************************************80
!
!! RANDOM_INITIALIZE initializes the FORTRAN90 random number seed.
!
!  Discussion:
!
!    If you don't initialize the random number generator, its behavior
!    is not specified.  If you initialize it simply by:
!
!      call random_seed ( )
!
!    its behavior is not specified.  On the DEC ALPHA, if that's all you
!    do, the same random number sequence is returned.  In order to actually
!    try to scramble up the random number generator a bit, this routine
!    goes through the tedious process of getting the size of the random
!    number seed, making up values based on the current time, and setting
!    the random number seed.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 December 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) SEED.
!    If SEED is zero on input, then you're asking this routine to come up
!    with a seed value, which is returned as output.
!    If SEED is nonzero on input, then you're asking this routine to
!    use the input value of SEED to initialize the random number generator,
!    and SEED is not changed on output.
!
  implicit none

  integer ( kind = 4 ) count
  integer ( kind = 4 ) count_max
  integer ( kind = 4 ) count_rate
  logical, parameter :: debug = .false.
  integer ( kind = 4 ) i
  integer ( kind = 4 ) seed
  integer ( kind = 4 ), allocatable :: seed_vector(:)
  integer ( kind = 4 ) seed_size
  real    ( kind = 4 ) t
!
!  Initialize the random number seed.
!
  call random_seed ( )
!
!  Determine the size of the random number seed.
!
  call random_seed ( size = seed_size )
!
!  Allocate a seed of the right size.
!
  allocate ( seed_vector(seed_size) )

  if ( seed /= 0 ) then

    if ( debug ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'RANDOM_INITIALIZE'
      write ( *, '(a,i20)' ) '  Initialize RANDOM_NUMBER, user SEED = ', seed
    end if

  else

    call system_clock ( count, count_rate, count_max )

    seed = count

    if ( debug ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'RANDOM_INITIALIZE'
      write ( *, '(a,i20)' ) '  Initialize RANDOM_NUMBER, arbitrary SEED = ', &
        seed
    end if

  end if
!
!  Now set the seed.
!
  seed_vector(1:seed_size) = seed

  call random_seed ( put = seed_vector(1:seed_size) )
!
!  Free up the seed space.
!
  deallocate ( seed_vector )
!
!  Call the random number routine a bunch of times.
!
  do i = 1, 100
    call random_number ( harvest = t )
  end do

  return
end
subroutine rcost ( n, x, wsave )

!*****************************************************************************80
!
!! RCOST computes the discrete Fourier cosine transform of an even sequence.
!
!  Discussion:
!
!    This routine is the unnormalized inverse of itself.  Two successive
!    calls will multiply the input sequence X by 2*(N-1).
!
!    The array WSAVE must be initialized by calling RCOSTI.
!
!    The transform is defined by:
!
!      X_out(I) = X_in(1) + (-1) **(I-1) * X_in(N) + sum ( 2 <= K <= N-1 )
!
!        2 * X_in(K) * cos ( ( K - 1 ) * ( I - 1 ) * PI / ( N - 1 ) )
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    Paul Swarztrauber,
!    Vectorizing the FFT's,
!    in Parallel Computations,
!    G. Rodrigue, editor,
!    Academic Press, 1982, pages 51-83.
!
!    Bill Buzbee,
!    The SLATEC Common Math Library,
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the sequence to be
!    transformed.  The method is more efficient when N-1 is the product of
!    small primes.
!
!    Input/output, real ( kind = 4 ) X(N).
!    On input, the sequence to be transformed.
!    On output, the transformed sequence.
!
!    Input, real ( kind = 4 ) WSAVE(3*N+15).
!    The WSAVE array must be initialized by calling RCOSTI.  A different
!    array must be used for each different value of N.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 4 ) c1
  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  integer ( kind = 4 ) kc
  integer ( kind = 4 ) ns2
  real    ( kind = 4 ) t1
  real    ( kind = 4 ) t2
  real    ( kind = 4 ) tx2
  real    ( kind = 4 ) wsave(3*n+15)
  real    ( kind = 4 ) x(n)
  real    ( kind = 4 ) x1h
  real    ( kind = 4 ) x1p3
  real    ( kind = 4 ) xi
  real    ( kind = 4 ) xim2

  ns2 = n / 2

  if ( n <= 1 ) then
    return
  end if

  if ( n == 2 ) then
    x1h = x(1) + x(2)
    x(2) = x(1) - x(2)
    x(1) = x1h
    return
  end if

  if ( n == 3 ) then
    x1p3 = x(1) + x(3)
    tx2 = x(2) + x(2)
    x(2) = x(1) - x(3)
    x(1) = x1p3 + tx2
    x(3) = x1p3 - tx2
    return
  end if

  c1 = x(1) - x(n)
  x(1) = x(1) + x(n)

  do k = 2, ns2
    kc = n + 1 - k
    t1 = x(k) + x(kc)
    t2 = x(k) - x(kc)
    c1 = c1 + wsave(kc) * t2
    t2 = wsave(k) * t2
    x(k) = t1 - t2
    x(kc) = t1 + t2
  end do

  if ( mod ( n, 2 ) /= 0 ) then
    x(ns2+1) = x(ns2+1) + x(ns2+1)
  end if

  call rfftf ( n-1, x, wsave(n+1) )

  xim2 = x(2)
  x(2) = c1

  do i = 4, n, 2
    xi = x(i)
    x(i) = x(i-2) - x(i-1)
    x(i-1) = xim2
    xim2 = xi
  end do

  if ( mod ( n, 2 ) /= 0 ) then
    x(n) = xim2
  end if

  return
end
subroutine rcosti ( n, wsave )

!*****************************************************************************80
!
!! RCOSTI initializes WSAVE, used in RCOST.
!
!  Discussion:
!
!    The prime factorization of N together with a tabulation of the
!    trigonometric functions are computed and stored in WSAVE.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    Paul Swarztrauber,
!    Vectorizing the FFT's,
!    in Parallel Computations,
!    G. Rodrigue, editor,
!    Academic Press, 1982, pages 51-83.
!
!    Bill Buzbee,
!    The SLATEC Common Math Library,
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the sequence to be
!    transformed.  The method is more efficient when N-1 is the product of
!    small primes.
!
!    Output, real ( kind = 4 ) WSAVE(3*N+15), contains data, depending on N, and
!    required by the RCOST algorithm.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 4 ) dt
  integer ( kind = 4 ) k
  real    ( kind = 4 ), parameter :: pi = 3.1415926E+00
  real    ( kind = 4 ) wsave(3*n+15)

  if ( n <= 3 ) then
    return
  end if

  dt = pi / real ( n - 1, kind = 4 )

  do k = 2, ( n / 2 )
    wsave(k)     = 2.0E+00 * sin ( real ( k - 1, kind = 4 ) * dt )
    wsave(n+1-k) = 2.0E+00 * cos ( real ( k - 1, kind = 4 ) * dt )
  end do

  call rffti ( n-1, wsave(n+1) )

  return
end
subroutine rfftb ( n, r, wsave )

!*****************************************************************************80
!
!! RFFTB computes a real periodic sequence from its Fourier coefficients.
!
!  Discussion:
!
!    This process is sometimes called Fourier synthesis.
!
!    The transform is unnormalized.  A call to RFFTF followed by a call to
!    RFFTB will multiply the input sequence by N.
!
!    If N is even, the transform is defined by:
!
!      R_out(I) = R_in(1) + (-1)**(I-1) * R_in(N) + sum ( 2 <= K <= N/2 )
!
!        + 2 * R_in(2*K-2) * cos ( ( K - 1 ) * ( I - 1 ) * 2 * PI / N )
!
!        - 2 * R_in(2*K-1) * sin ( ( K - 1 ) * ( I - 1 ) * 2 * PI / N )
!
!    If N is odd, the transform is defined by:
!
!      R_out(I) = R_in(1) + sum ( 2 <= K <= (N+1)/2 )
!
!        + 2 * R_in(2*K-2) * cos ( ( K - 1 ) * ( I - 1 ) * 2 * PI / N )
!
!        - 2 * R_in(2*K-1) * sin ( ( K - 1 ) * ( I - 1 ) * 2 * PI / N )
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    Paul Swarztrauber,
!    Vectorizing the FFT's,
!    in Parallel Computations,
!    G. Rodrigue, editor,
!    Academic Press, 1982, pages 51-83.
!
!    Bill Buzbee,
!    The SLATEC Common Math Library,
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the array to be transformed.
!    The method is more efficient when N is the product of small primes.
!
!    Input/output, real ( kind = 4 ) R(N).
!    On input, the sequence to be transformed.
!    On output, the transformed sequence.
!
!    Input, real ( kind = 4 ) WSAVE(2*N+15), a work array.  The WSAVE array must be
!    initialized by calling RFFTI.  A different WSAVE array must be used
!    for each different value of N.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 4 ) r(n)
  real    ( kind = 4 ) wsave(2*n+15)

  if ( n <= 1 ) then
    return
  end if

  call rfftb1 ( n, r, wsave(1), wsave(n+1), wsave(2*n+1) )

  return
end
subroutine rfftb1 ( n, c, ch, wa, ifac )

!*****************************************************************************80
!
!! RFFTB1 is a lower level routine used by RFFTB.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the array to be transformed.
!
!    Input/output, real ( kind = 4 ) C(N).
!    On input, the sequence to be transformed.
!    On output, the transformed sequence.
!
!    Input, real ( kind = 4 ) CH(N).
!
!    Input, real ( kind = 4 ) WA(N).
!
!    Input, integer ( kind = 4 ) IFAC(15).
!    IFAC(1) = N, the number that was factored.
!    IFAC(2) = NF, the number of factors.
!    IFAC(3:2+NF), the factors.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 4 ) c(n)
  real    ( kind = 4 ) ch(n)
  integer ( kind = 4 ) idl1
  integer ( kind = 4 ) ido
  integer ( kind = 4 ) ifac(15)
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) iw
  integer ( kind = 4 ) ix2
  integer ( kind = 4 ) ix3
  integer ( kind = 4 ) ix4
  integer ( kind = 4 ) k1
  integer ( kind = 4 ) l1
  integer ( kind = 4 ) l2
  integer ( kind = 4 ) na
  integer ( kind = 4 ) nf
  real    ( kind = 4 ) wa(n)

  nf = ifac(2)
  na = 0
  l1 = 1
  iw = 1

  do k1 = 1, nf

    ip = ifac(k1+2)
    l2 = ip * l1
    ido = n / l2
    idl1 = ido * l1

    if ( ip == 4 ) then

      ix2 = iw + ido
      ix3 = ix2 + ido

      if ( na == 0 ) then
        call radb4 ( ido, l1, c, ch, wa(iw), wa(ix2), wa(ix3) )
      else
        call radb4 ( ido, l1, ch, c, wa(iw), wa(ix2), wa(ix3) )
      end if

      na = 1 - na

    else if ( ip == 2 ) then

      if ( na == 0 ) then
        call radb2 ( ido, l1, c, ch, wa(iw) )
      else
        call radb2 ( ido, l1, ch, c, wa(iw) )
      end if

      na = 1 - na

    else if ( ip == 3 ) then

      ix2 = iw + ido

      if ( na == 0 ) then
        call radb3 ( ido, l1, c, ch, wa(iw), wa(ix2) )
      else
        call radb3 ( ido, l1, ch, c, wa(iw), wa(ix2) )
      end if

      na = 1 - na

    else if ( ip == 5 ) then

      ix2 = iw + ido
      ix3 = ix2 + ido
      ix4 = ix3 + ido

      if ( na == 0 ) then
        call radb5 ( ido, l1, c, ch, wa(iw), wa(ix2), wa(ix3), wa(ix4) )
      else
        call radb5 ( ido, l1, ch, c, wa(iw), wa(ix2), wa(ix3), wa(ix4) )
      end if

      na = 1 - na

    else

      if ( na == 0 ) then
        call radbg ( ido, ip, l1, idl1, c, c, c, ch, ch, wa(iw) )
      else
        call radbg ( ido, ip, l1, idl1, ch, ch, ch, c, c, wa(iw) )
      end if

      if ( ido == 1 ) then
        na = 1 - na
      end if

    end if

    l1 = l2
    iw = iw + ( ip - 1 ) * ido

  end do

  if ( na /= 0 ) then
    c(1:n) = ch(1:n)
  end if

  return
end
subroutine rfftf ( n, r, wsave )

!*****************************************************************************80
!
!! RFFTF computes the Fourier coefficients of a real periodic sequence.
!
!  Discussion:
!
!    This process is sometimes called Fourier analysis.
!
!    The transform is unnormalized.  A call to RFFTF followed by a call
!    to RFFTB will multiply the input sequence by N.
!
!    The transform is defined by:
!
!      R_out(1) = sum ( 1 <= I <= N ) R_in(I)
!
!    Letting L = (N+1)/2, then for 2 <= K <= L
!
!      R_out(2*K-2) = sum ( 1 <= I <= N )
!
!        R_in(I) * cos ( ( K - 1 ) * ( I - 1 ) * 2 * PI / N )
!
!      R_out(2*K-1) = sum ( 1 <= I <= N )
!
!        -R_in(I) * sin ( ( K - 1 ) * ( I - 1 ) * 2 * PI / N )
!
!    And, if N is even, then:
!
!      R_out(N) = sum ( 1 <= I <= N ) (-1)**(I-1) * R_in(I)
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    Paul Swarztrauber,
!    Vectorizing the FFT's,
!    in Parallel Computations,
!    G. Rodrigue, editor,
!    Academic Press, 1982, pages 51-83.
!
!    Bill Buzbee,
!    The SLATEC Common Math Library,
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the array to be transformed.
!    The method is more efficient when N is the product of small primes.
!
!    Input/output, real ( kind = 4 ) R(N).
!    On input, the sequence to be transformed.
!    On output, the transformed sequence.
!
!    Input, real ( kind = 4 ) WSAVE(2*N+15), a work array.  The WSAVE array must be
!    initialized by calling RFFTI.  A different WSAVE array must be used
!    for each different value of N.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 4 ) r(n)
  real    ( kind = 4 ) wsave(2*n+15)

  if ( n <= 1 ) then
    return
  end if

  call rfftf1 ( n, r, wsave(1), wsave(n+1), wsave(2*n+1) )

  return
end
subroutine rfftf1 ( n, c, ch, wa, ifac )

!*****************************************************************************80
!
!! RFFTF1 is a lower level routine used by RFFTF and RSINT.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    12 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the array to be transformed.
!
!    Input/output, real ( kind = 4 ) C(N).
!    On input, the sequence to be transformed.
!    On output, the transformed sequence.
!
!    Input, real ( kind = 4 ) CH(N).
!
!    Input, real ( kind = 4 ) WA(N).
!
!    Input, integer ( kind = 4 ) IFAC(15).
!    IFAC(1) = N, the number that was factored.
!    IFAC(2) = NF, the number of factors.
!    IFAC(3:2+NF), the factors.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 4 ) c(n)
  real    ( kind = 4 ) ch(n)
  integer ( kind = 4 ) idl1
  integer ( kind = 4 ) ido
  integer ( kind = 4 ) ifac(15)
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) iw
  integer ( kind = 4 ) ix2
  integer ( kind = 4 ) ix3
  integer ( kind = 4 ) ix4
  integer ( kind = 4 ) k1
  integer ( kind = 4 ) kh
  integer ( kind = 4 ) l1
  integer ( kind = 4 ) l2
  integer ( kind = 4 ) na
  integer ( kind = 4 ) nf
  real    ( kind = 4 ) wa(n)

  nf = ifac(2)
  na = 1
  l2 = n
  iw = n

  do k1 = 1, nf

    kh = nf - k1
    ip = ifac(kh+3)
    l1 = l2 / ip
    ido = n / l2
    idl1 = ido * l1
    iw = iw - ( ip - 1 ) * ido
    na = 1 - na

    if ( ip == 4 ) then

      ix2 = iw + ido
      ix3 = ix2 + ido

      if ( na == 0 ) then
        call radf4 ( ido, l1, c, ch, wa(iw), wa(ix2), wa(ix3) )
      else
        call radf4 ( ido, l1, ch, c, wa(iw), wa(ix2), wa(ix3) )
      end if

    else if ( ip == 2 ) then

      if ( na == 0 ) then
        call radf2 ( ido, l1, c, ch, wa(iw) )
      else
        call radf2 ( ido, l1, ch, c, wa(iw) )
      end if

    else if ( ip == 3 ) then

      ix2 = iw + ido

      if ( na == 0 ) then
        call radf3 ( ido, l1, c, ch, wa(iw), wa(ix2) )
      else
        call radf3 ( ido, l1, ch, c, wa(iw), wa(ix2) )
      end if

    else if ( ip == 5 ) then

      ix2 = iw + ido
      ix3 = ix2 + ido
      ix4 = ix3 + ido

      if ( na == 0 ) then
        call radf5 ( ido, l1, c, ch, wa(iw), wa(ix2), wa(ix3), wa(ix4) )
      else
        call radf5 ( ido, l1, ch, c, wa(iw), wa(ix2), wa(ix3), wa(ix4) )
      end if

    else

      if ( ido == 1 ) then
        na = 1 - na
      end if

      if ( na == 0 ) then
        call radfg ( ido, ip, l1, idl1, c, c, c, ch, ch, wa(iw) )
        na = 1
      else
        call radfg ( ido, ip, l1, idl1, ch, ch, ch, c, c, wa(iw) )
        na = 0
      end if

    end if

    l2 = l1

  end do

  if ( na /= 1 ) then
    c(1:n) = ch(1:n)
  end if

  return
end
subroutine rffti ( n, wsave )

!*****************************************************************************80
!
!! RFFTI initializes WSAVE, used in RFFTF and RFFTB.
!
!  Discussion:
!
!    The prime factorization of N together with a tabulation of the
!    trigonometric functions are computed and stored in WSAVE.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    Paul Swarztrauber,
!    Vectorizing the FFT's,
!    in Parallel Computations,
!    G. Rodrigue, editor,
!    Academic Press, 1982, pages 51-83.
!
!    Bill Buzbee,
!    The SLATEC Common Math Library,
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the sequence to be
!    transformed.
!
!    Output, real ( kind = 4 ) WSAVE(2*N+15), contains data, dependent on the value
!    of N, which is necessary for the RFFTF and RFFTB routines.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 4 ) wsave(2*n+15)

  if ( n <= 1 ) then
    return
  end if

  call rffti1 ( n, wsave(n+1), wsave(2*n+1) )

  return
end
subroutine rffti1 ( n, wa, ifac )

!*****************************************************************************80
!
!! RFFTI1 is a lower level routine used by RFFTI.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the sequence to be transformed.
!
!    Input, real ( kind = 4 ) WA(N).
!
!    Input, integer ( kind = 4 ) IFAC(15).
!    IFAC(1) = N, the number that was factored.
!    IFAC(2) = NF, the number of factors.
!    IFAC(3:2+NF), the factors.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 4 ) arg
  real    ( kind = 4 ) argh
  real    ( kind = 4 ) argld
  real    ( kind = 4 ) fi
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ido
  integer ( kind = 4 ) ifac(15)
  integer ( kind = 4 ) ii
  integer ( kind = 4 ) ip
  integer ( kind = 4 ) is
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k1
  integer ( kind = 4 ) l1
  integer ( kind = 4 ) l2
  integer ( kind = 4 ) ld
  integer ( kind = 4 ) nf
  real    ( kind = 4 ), parameter :: pi = 3.1415926E+00
  real    ( kind = 4 ) wa(n)

  call i4_factor ( n, ifac )

  nf = ifac(2)

  argh = 2.0E+00 * pi / real ( n, kind = 4 )
  is = 0
  l1 = 1

  do k1 = 1, nf-1

    ip = ifac(k1+2)
    ld = 0
    l2 = l1 * ip
    ido = n / l2

    do j = 1, ip-1

      ld = ld + l1
      i = is
      argld = real ( ld, kind = 4 ) * argh
      fi = 0.0E+00

      do ii = 3, ido, 2
        i = i + 2
        fi = fi + 1.0E+00
        arg = fi * argld
        wa(i-1) = cos ( arg )
        wa(i) = sin ( arg )
      end do

      is = is + ido

    end do

    l1 = l2

  end do

  return
end
subroutine sinqb ( n, x, wsave )

!*****************************************************************************80
!
!! SINQB computes the fast sine transform of quarter wave data.
!
!  Discussion:
!
!    SINQB computes a sequence from its representation in terms of a sine
!    series with odd wave numbers.
!
!    SINQF is the unnormalized inverse of SINQB since a call of SINQB
!    followed by a call of SINQF will multiply the input sequence X by 4*N.
!
!    The array WSAVE must be initialized by calling SINQI.
!
!    The transform is defined by:
!
!      X_out(I) = sum ( 1 <= K <= N )
!
!        4 * X_in(K) * sin ( ( 2 * K - 1 ) * I * PI / ( 2 * N ) )
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    12 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    Paul Swarztrauber,
!    Vectorizing the FFT's,
!    in Parallel Computations,
!    G. Rodrigue, editor,
!    Academic Press, 1982, pages 51-83.
!
!    Bill Buzbee,
!    The SLATEC Common Math Library,
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the array to be transformed.
!    The method is more efficient when N is the product of small primes.
!
!    Input/output, real ( kind = 4 ) X(N).
!    On input, the sequence to be transformed.
!    On output, the transformed sequence.
!
!    Input, real ( kind = 4 ) WSAVE(3*N+15), a work array.  The WSAVE array must be
!    initialized by calling SINQI.  A different WSAVE array must be used
!    for each different value of N.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 4 ) wsave(3*n+15)
  real    ( kind = 4 ) x(n)

  if ( n < 1 ) then
    return
  end if

  if ( n == 1 ) then
    x(1) = 4.0E+00 * x(1)
    return
  end if

  x(2:n:2) = -x(2:n:2)

  call cosqb ( n, x, wsave )
!
!  Reverse the X vector.
!
  call r4vec_reverse ( n, x )

  return
end
subroutine sinqf ( n, x, wsave )

!*****************************************************************************80
!
!! SINQF computes the fast sine transform of quarter wave data.
!
!  Discussion:
!
!    SINQF computes the coefficients in a sine series representation with
!    only odd wave numbers.
!
!    SINQB is the unnormalized inverse of SINQF since a call of SINQF
!    followed by a call of SINQB will multiply the input sequence X by 4*N.
!
!    The array WSAVE, which is used by SINQF, must be initialized by
!    calling SINQI.
!
!    The transform is defined by:
!
!      X_out(I) = (-1)**(I-1) * X_in(N) + sum ( 1 <= K <= N-1 )
!        2 * X_in(K) * sin ( ( 2 * I - 1 ) * K * PI / ( 2 * N ) )
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    Paul Swarztrauber,
!    Vectorizing the FFT's,
!    in Parallel Computations,
!    G. Rodrigue, editor,
!    Academic Press, 1982, pages 51-83.
!
!    Bill Buzbee,
!    The SLATEC Common Math Library,
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the array to be transformed.  The
!    method is more efficient when N is the product of small primes.
!
!    Input/output, real ( kind = 4 ) X(N).
!    On input, the sequence to be transformed.
!    On output, the transformed sequence.
!
!    Input, real ( kind = 4 ) WSAVE(3*N+15), a work array.  The WSAVE array
!    must be initialized by calling SINQI.  A different WSAVE array must be
!    used for each different value of N.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 4 ) wsave(3*n+15)
  real    ( kind = 4 ) x(n)

  if ( n <= 1 ) then
    return
  end if
!
!  Reverse the X vector.
!
  call r4vec_reverse ( n, x )

  call cosqf ( n, x, wsave )

  x(2:n:2) = -x(2:n:2)

  return
end
subroutine sinqi ( n, wsave )

!*****************************************************************************80
!
!! SINQI initializes WSAVE, used in SINQF and SINQB.
!
!  Discussion:
!
!    The prime factorization of N together with a tabulation of the
!    trigonometric functions are computed and stored in WSAVE.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    Paul Swarztrauber,
!    Vectorizing the FFT's,
!    in Parallel Computations,
!    G. Rodrigue, editor,
!    Academic Press, 1982, pages 51-83.
!
!    Bill Buzbee,
!    The SLATEC Common Math Library,
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the array to be transformed.
!
!    Output, real ( kind = 4 ) WSAVE(3*N+15), contains data, dependent on the value
!    of N, which is necessary for the SINQF or SINQB routines.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 4 ) wsave(3*n+15)

  call cosqi ( n, wsave )

  return
end
subroutine rsint ( n, x, wsave )

!*****************************************************************************80
!
!! RSINT computes the discrete Fourier sine transform of an odd sequence.
!
!  Discussion:
!
!    This routine is the unnormalized inverse of itself since two successive
!    calls will multiply the input sequence X by 2*(N+1).
!
!    The array WSAVE must be initialized by calling RSINTI.
!
!    The transform is defined by:
!
!      X_out(I) = sum ( 1 <= K <= N )
!        2 * X_in(K) * sin ( K * I * PI / ( N + 1 ) )
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    Paul Swarztrauber,
!    Vectorizing the FFT's,
!    in Parallel Computations,
!    G. Rodrigue, editor,
!    Academic Press, 1982, pages 51-83.
!
!    Bill Buzbee,
!    The SLATEC Common Math Library,
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the sequence to be
!    transformed.  The method is most efficient when N+1 is the product of
!    small primes.
!
!    Input/output, real ( kind = 4 ) X(N).
!    On input, the sequence to be transformed.
!    On output, the transformed sequence.
!
!    Input, real ( kind = 4 ) WSAVE((5*N+30)/2), a work array.  The WSAVE array
!    must be initialized by calling RSINTI.  A different WSAVE array must be
!    used for each different value of N.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) iw1
  integer ( kind = 4 ) iw2
  integer ( kind = 4 ) iw3
  real    ( kind = 4 ) wsave((5*n+30)/2)
  real    ( kind = 4 ) x(n)

  iw1 = n / 2 + 1
  iw2 = iw1 + n + 1
  iw3 = iw2 + n + 1

  call rsint1 ( n, x, wsave(1), wsave(iw1), wsave(iw2), wsave(iw3) )

  return
end
subroutine rsint1 ( n, war, was, xh, x, ifac )

!*****************************************************************************80
!
!! RSINT1 is a lower level routine used by RSINT.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the sequence to be
!    transformed.
!
!    Input/output, real ( kind = 4 ) WAR(N).
!    On input, the sequence to be transformed.
!    On output, the transformed sequence.
!
!    Input, real ( kind = 4 ) WAS(N/2).
!
!    Input, real ( kind = 4 ) XH(N).
!
!    Input, real ( kind = 4 ) X(N+1), ?.
!
!    Input, integer ( kind = 4 ) IFAC(15).
!    IFAC(1) = N, the number that was factored.
!    IFAC(2) = NF, the number of factors.
!    IFAC(3:2+NF), the factors.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ifac(15)
  integer ( kind = 4 ) k
  integer ( kind = 4 ) ns2
  real    ( kind = 4 ), parameter :: sqrt3 = 1.73205080756888E+00
  real    ( kind = 4 ) t1
  real    ( kind = 4 ) t2
  real    ( kind = 4 ) war(n)
  real    ( kind = 4 ) was(n/2)
  real    ( kind = 4 ) x(n+1)
  real    ( kind = 4 ) xh(n)
  real    ( kind = 4 ) xhold

  xh(1:n) = war(1:n)
  war(1:n) = x(1:n)

  if ( n <= 1 ) then
    xh(1) = 2.0E+00 * xh(1)
    return
  end if

  if ( n == 2 ) then
    xhold = sqrt3 * ( xh(1) + xh(2) )
    xh(2) = sqrt3 * ( xh(1) - xh(2) )
    xh(1) = xhold
    return
  end if

  ns2 = n / 2
  x(1) = 0.0E+00

  do k = 1, n/2
    t1 = xh(k) - xh(n+1-k)
    t2 = was(k) * ( xh(k) + xh(n+1-k) )
    x(k+1) = t1 + t2
!
! ??? N+2-K puts us out of the array...DAMN IT, THIS IS AN ERROR.
!
    x(n+2-k) = t2 - t1
  end do

  if ( mod ( n, 2 ) /= 0 ) then
    x(n/2+2) = 4.0E+00 * xh(n/2+1)
  end if
!
!  This call says there are N+1 things in X.
!
  call rfftf1 ( n+1, x, xh, war, ifac )

  xh(1) = 0.5E+00 * x(1)
  do i = 3, n, 2
    xh(i-1) = -x(i)
    xh(i) = xh(i-2) + x(i-1)
  end do

  if ( mod ( n, 2 ) == 0 ) then
    xh(n) = -x(n+1)
  end if

  x(1:n) = war(1:n)
  war(1:n) = xh(1:n)

  return
end
subroutine rsinti ( n, wsave )

!*****************************************************************************80
!
!! RSINTI initializes WSAVE, used in RSINT.
!
!  Discussion:
!
!    The prime factorization of N together with a tabulation of the
!    trigonometric functions are computed and stored in WSAVE.
!
!  Licensing:
!
!    This code is a translation into FORTRAN90 of the corresponding code
!    in version 4.0 of the FORTRAN77 FFTPACK library, which is in the
!    public domain.
!
!  Modified:
!
!    09 March 2001
!
!  Author:
!
!    Paul Swarztrauber,
!    National Center for Atmospheric Research
!
!  Reference:
!
!    David Kahaner, Cleve Moler, Steven Nash,
!    Numerical Methods and Software,
!    Prentice Hall, 1988.
!
!    Paul Swarztrauber,
!    Vectorizing the FFT's,
!    in Parallel Computations,
!    G. Rodrigue, editor,
!    Academic Press, 1982, pages 51-83.
!
!    Bill Buzbee,
!    The SLATEC Common Math Library,
!    in Sources and Development of Mathematical Software,
!    W. Cowell, editor,
!    Prentice Hall, 1984, pages 302-318.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the length of the sequence to be transformed.
!    The method is most efficient when N+1 is a product of small primes.
!
!    Output, real ( kind = 4 ) WSAVE((5*N+30)/2), contains data, dependent
!    on the value of N, which is necessary for the RSINT routine.
!
  implicit none

  integer ( kind = 4 ) n

  real    ( kind = 4 ) dt
  integer ( kind = 4 ) k
  real    ( kind = 4 ), parameter :: pi = 3.1415926E+00
  real    ( kind = 4 ) wsave((5*n+30)/2)

  if ( n <= 1 ) then
    return
  end if

  dt = pi / real ( n + 1, kind = 4 )

  do k = 1, n/2
    wsave(k) = 2.0E+00 * sin ( real ( k, kind = 4 ) * dt )
  end do

  call rffti ( n+1, wsave((n/2)+1) )

  return
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    May 31 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none

  character ( len = 8 )  ampm
  integer   ( kind = 4 ) d
  character ( len = 8 )  date
  integer   ( kind = 4 ) h
  integer   ( kind = 4 ) m
  integer   ( kind = 4 ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer   ( kind = 4 ) n
  integer   ( kind = 4 ) s
  character ( len = 10 ) time
  integer   ( kind = 4 ) values(8)
  integer   ( kind = 4 ) y
  character ( len = 5 )  zone

  call date_and_time ( date, time, zone, values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(a,1x,i2,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    trim ( month(m) ), d, y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
