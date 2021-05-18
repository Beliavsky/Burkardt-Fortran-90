function dzasum ( n, x, incx )

!*****************************************************************************80
!
!! DZASUM takes the sum of the absolute values of a vector.
!
!  Discussion:
!
!    This routine uses double precision complex arithmetic.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 April 2006
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for FORTRAN usage,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, pages 308-323, 1979.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, complex ( kind = 8 ) X(*), the vector.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive 
!    entries of X.
!
!    Output, real ( kind = 8 ) DZASUM, the sum of the absolute values.
!
  implicit none

  real ( kind = 8 ) dzasum
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nincx
  complex ( kind = 8 ) x(*)

  dzasum = 0.0D+00

  if ( n <= 0 .or. incx <= 0 ) then
    return
  end if

  if ( incx == 1 ) then

    dzasum = sum ( abs ( real ( x(1:n), kind = 8 ) ) &
                 + abs ( aimag ( x(1:n) ) ) )

  else

    nincx = n * incx

    dzasum = sum ( abs ( real ( x(1:nincx:incx), kind = 8 ) ) &
                 + abs ( aimag ( x(1:nincx:incx) ) ) )

  end if

  return
end
function dznrm2 ( n, x, incx )

!*****************************************************************************80
!
!! DZNRM2 returns the euclidean norm of a vector.
!
!  Discussion:
!
!    This routine uses double precision complex arithmetic.
!
!    DZNRM2 := sqrt ( sum ( conjg ( x(1:n) ) * x(1:n) ) )
!            = sqrt ( dot_product ( x(1:n), x(1:n) ) )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 April 2006
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for FORTRAN usage,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, pages 308-323, 1979.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, complex ( kind = 8 ) X(*), the vector.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of X.
!
!    Output, real ( kind = 8 ) DZNRM2, the norm of the vector.
!
  implicit none

  real ( kind = 8 ) dznrm2
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) n
  real ( kind = 8 ) norm
  real ( kind = 8 ) scale
  real ( kind = 8 ) ssq
  real ( kind = 8 ) temp
  complex ( kind = 8 ) x(*)

  if ( n < 1 .or. incx < 1 ) then

    norm = 0.0D+00

  else

    scale = 0.0D+00
    ssq = 1.0D+00

    do ix = 1, 1 + ( n - 1 ) * incx, incx

      if ( real ( x(ix), kind = 8 ) /= 0.0D+00 ) then
        temp = abs ( real ( x(ix), kind = 8 ) )
        if ( scale < temp ) then
          ssq = 1.0D+00 + ssq * ( scale / temp )**2
          scale = temp
        else
          ssq = ssq + ( temp / scale )**2
        end if
      end if

      if ( aimag ( x(ix) ) /= 0.0D+00 ) then
        temp = abs ( aimag ( x(ix) ) )
        if ( scale < temp ) then
          ssq = 1.0D+00 + ssq * ( scale / temp )**2
          scale = temp
        else
          ssq = ssq + ( temp / scale )**2
        end if

      end if

    end do

    norm  = scale * sqrt ( ssq )

  end if

  dznrm2 = norm

  return
end
function izamax ( n, x, incx )

!*****************************************************************************80
!
!! IZAMAX indexes the element of maximum absolute value.
!
!  Discussion:
!
!    This routine uses double precision complex arithmetic.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 April 2006
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for FORTRAN usage,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, pages 308-323, 1979.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, complex ( kind = 8 ) X(*), the vector.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive 
!    entries of X.
!
!    Output, integer ( kind = 4 ) IZAMAX, the index of the element of maximum
!    absolute value.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) izamax
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) n
  real ( kind = 8 ) smax
  complex ( kind = 8 ) x(*)
  real ( kind = 8 ) zabs1

  izamax = 0
  if ( n < 1 .or. incx  <=  0 ) then
    return
  end if

  izamax = 1

  if ( n == 1 ) then
    return
  end if

  if ( incx /= 1 ) then

    ix = 1
    smax = zabs1 ( x(1) )
    ix = ix + incx
    do i = 2, n
      if ( smax < zabs1 ( x(ix) ) ) then
        izamax = i
        smax = zabs1 ( x(ix) )
      end if
      ix = ix + incx
    end do

  else

    smax = zabs1 ( x(1) )
    do i = 2, n
      if ( smax < zabs1 ( x(i) ) ) then
        izamax = i
        smax = zabs1 ( x(i) )
      end if
    end do

  end if

  return
end
subroutine zaxpy ( n, ca, cx, incx, cy, incy )

!*****************************************************************************80
!
!! ZAXPY computes a constant times a vector plus a vector.
!
!  Discussion:
!
!    This routine uses double precision complex arithmetic.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    20 April 2007
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements in CX and CY.
!
!    Input, complex ( kind = 8 ) CA, the multiplier of CX.
!
!    Input, complex ( kind = 8 ) CX(*), the first vector.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive 
!    entries of CX.
!
!    Input/output, complex ( kind = 8 ) CY(*), the second vector.
!    On output, CY(*) has been replaced by CY(*) + CA * CX(*).
!
!    Input, integer ( kind = 4 ) INCY, the increment between successive 
!    entries of CY.
!
  implicit none

  complex ( kind = 8 ) ca
  complex ( kind = 8 ) cx(*)
  complex ( kind = 8 ) cy(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) n
  real ( kind = 8 ) zabs1

  if ( n <= 0 ) then
    return
  end if

  if ( zabs1 ( ca ) == 0.0D+00 ) then
    return
  end if

  if ( incx /= 1 .or. incy /= 1 ) then

    if ( 0 <= incx ) then
      ix = 1
    else
      ix = ( -n + 1 ) * incx + 1
    end if

    if ( 0 <= incy ) then
      iy = 1
    else
      iy = ( -n + 1 ) * incy + 1
    end if

    do i = 1, n
      cy(iy) = cy(iy) + ca * cx(ix)
      ix = ix + incx
      iy = iy + incy
    end do

  else

    cy(1:n) = cy(1:n) + ca * cx(1:n)

  end if

  return
end
subroutine zcopy ( n, cx, incx, cy, incy )

!*****************************************************************************80
!
!! ZCOPY copies a vector X to a vector Y.
!
!  Discussion:
!
!    This routine uses double precision complex arithmetic.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 April 2006
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of elements in CX and CY.
!
!    Input, complex ( kind = 8 ) CX(*), the first vector.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive 
!    entries of CX.
!
!    Output, complex ( kind = 8 ) CY(*), the second vector.
!
!    Input, integer ( kind = 4 ) INCY, the increment between successive 
!    entries of CY.
!
  implicit none

  complex ( kind = 8 ) cx(*)
  complex ( kind = 8 ) cy(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) n

  if ( n <= 0 ) then
    return
  end if

  if ( incx /= 1 .or. incy /= 1 ) then

    if ( 0 <= incx ) then
      ix = 1
    else
      ix = ( -n + 1 ) * incx + 1
    end if

    if ( 0 <= incy ) then
      iy = 1
    else
      iy = ( -n + 1 ) * incy + 1
    end if

    do i = 1, n
      cy(iy) = cx(ix)
      ix = ix + incx
      iy = iy + incy
    end do

  else

    cy(1:n) = cx(1:n)

  end if

  return
end
function zdotc ( n, cx, incx, cy, incy )

!*****************************************************************************80
!
!! ZDOTC forms the conjugated dot product of two vectors.
!
!  Discussion:
!
!    This routine uses double precision complex arithmetic.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 April 2006
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vectors.
!
!    Input, complex ( kind = 8 ) CX(*), the first vector.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive 
!    entries in CX.
!
!    Input, complex ( kind = 8 ) CY(*), the second vector.
!
!    Input, integer ( kind = 4 ) INCY, the increment between successive 
!    entries in CY.
!
!    Output, complex ( kind = 8 ) ZDOTC, the conjugated dot product of 
!    the corresponding entries of CX and CY.
!
  implicit none

  complex ( kind = 8 ) ctemp
  complex ( kind = 8 ) cx(*)
  complex ( kind = 8 ) cy(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) n
  complex ( kind = 8 ) zdotc

  ctemp = cmplx ( 0.0D+00, 0.0D+00, kind = 8 )
  zdotc = cmplx ( 0.0D+00, 0.0D+00, kind = 8 )

  if ( n <= 0 ) then
    return
  end if

  if ( incx == 1 .and. incy == 1 ) then

    do i = 1, n
      ctemp = ctemp + conjg ( cx(i) ) * cy(i)
    end do

  else

    if ( 0 <= incx ) then
      ix = 1
    else
      ix = ( -n + 1 ) * incx + 1
    end if

    if ( 0 <= incy ) then
      iy = 1
    else
      iy = ( -n + 1 ) * incy + 1
    end if

    do i = 1, n
      ctemp = ctemp + conjg ( cx(ix) ) * cy(iy)
      ix = ix + incx
      iy = iy + incy
    end do

  end if

  zdotc = ctemp

  return
end
function zdotu ( n, cx, incx, cy, incy )

!*****************************************************************************80
!
!! ZDOTU forms the unconjugated dot product of two vectors.
!
!  Discussion:
!
!    This routine uses double precision complex arithmetic.
!
!    Using the FORTRAN90 function DOT_PRODUCT with complex vectors
!    as arguments will give you the conjugated dot product!
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 April 2006
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vectors.
!
!    Input, complex ( kind = 8 ) CX(*), the first vector.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive 
!    entries in CX.
!
!    Input, complex ( kind = 8 ) CY(*), the second vector.
!
!    Input, integer ( kind = 4 ) INCY, the increment between successive 
!    entries in CY.
!
!    Output, complex ( kind = 8 ) ZDOTU, the unconjugated dot product of 
!    the corresponding entries of CX and CY.
!
  implicit none

  complex ( kind = 8 ) ctemp
  complex ( kind = 8 ) cx(*)
  complex ( kind = 8 ) cy(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) n
  complex ( kind = 8 ) zdotu

  ctemp = cmplx ( 0.0D+00, 0.0D+00, kind = 8 )
  zdotu = cmplx ( 0.0D+00, 0.0D+00, kind = 8 )

  if ( n <= 0 ) then
    return
  end if

  if ( incx == 1 .and. incy == 1 ) then

    do i = 1, n
      ctemp = ctemp + cx(i) * cy(i)
    end do

  else

    if ( 0 <= incx ) then
      ix = 1
    else
      ix = ( -n + 1 ) * incx + 1
    end if

    if ( 0 <= incy ) then
      iy = 1
    else
      iy = ( -n + 1 ) * incy + 1
    end if

    do i = 1, n
      ctemp = ctemp + cx(ix) * cy(iy)
      ix = ix + incx
      iy = iy + incy
    end do

  end if

  zdotu = ctemp

  return
end
subroutine zdrot ( n, cx, incx, cy, incy, c, s )

!*****************************************************************************80
!
!! ZDROT applies a real plane rotation to double precision complex vectors.
!
!  Discussion:
!
!    The cosine C and sine S are double precision real, while the 
!    vectors CX and CY are double precision complex.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 April 2006
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vectors.
!
!    Input/output, complex ( kind = 8 ) CX(*), one of the vectors to be rotated.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive 
!    entries of CX.
!
!    Input/output, complex ( kind = 8 ) CY(*), one of the vectors to be rotated.
!
!    Input, integer ( kind = 4 ) INCY, the increment between successive 
!    elements of CY.
!
!    Input, real ( kind = 8 ) C, S, parameters (presumably the cosine and 
!    sine of some angle) that define a plane rotation.
!
  implicit none

  real ( kind = 8 ) c
  complex ( kind = 8 ) ctemp
  complex ( kind = 8 ) cx(*)
  complex ( kind = 8 ) cy(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) n
  real ( kind = 8 ) s

  if ( n <= 0 ) then
    return
  end if

  if ( incx == 1 .and. incy == 1 ) then

    do i = 1, n
      ctemp = c * cx(i) + s * cy(i)
      cy(i) = c * cy(i) - s * cx(i)
      cx(i) = ctemp
    end do

  else

    if ( 0 <= incx ) then
      ix = 1
    else
      ix = ( -n + 1 ) * incx + 1
    end if

    if ( 0 <= incy ) then
      iy = 1
    else
      iy = ( -n + 1 ) * incy + 1
    end if

    do i = 1, n
      ctemp  = c * cx(ix) + s * cy(iy)
      cy(iy) = c * cy(iy) - s * cx(ix)
      cx(ix) = ctemp
      ix = ix + incx
      iy = iy + incy
    end do

  end if

  return
end
subroutine zdscal ( n, sa, cx, incx )

!*****************************************************************************80
!
!! ZDSCAL scales a vector by a constant.
!
!  Discussion:
!
!    This routine uses double precision complex arithmetic.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 April 2006
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, real ( kind = 8 ) SA, the multiplier.
!
!    Input/output, complex ( kind = 8 ) CX(*), the vector to be scaled.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive 
!    entries of the vector CX.
!
  implicit none

  complex ( kind = 8 ) cx(*)
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nincx
  real ( kind = 8 ) sa

  if ( n <= 0 .or. incx <= 0 ) then
    return
  end if

  if ( incx == 1 ) then

    cx(1:n) = sa * cx(1:n)

  else

    nincx = n * incx

    cx(1:nincx:incx) = sa * cx(1:nincx:incx)

  end if

  return
end
subroutine zrotg ( ca, cb, c, s )

!*****************************************************************************80
!
!! ZROTG determines a Givens rotation.
!
!  Discussion:
!
!    This routine uses double precision complex arithmetic.
!
!    Given values A and B, this routine computes:
!
!    If A = 0:
!
!      R = B
!      C = 0
!      S = (1,0).
!
!    If A /= 0:
!
!      ALPHA = A / abs ( A )
!      NORM  = sqrt ( ( abs ( A ) )^2 + ( abs ( B ) )^2 )
!      R     = ALPHA * NORM
!      C     = abs ( A ) / NORM
!      S     = ALPHA * conj ( B ) / NORM
!
!    In either case, the computed numbers satisfy the equation:
!
!    (         C    S ) * ( A ) = ( R )
!    ( -conj ( S )  C )   ( B ) = ( 0 )
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    16 May 2006
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for FORTRAN usage,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, pages 308-323, 1979.
!
!  Parameters:
!
!    Input/output, complex ( kind = 8 ) CA, on input, the value A.  On output,
!    the value R.
!
!    Input, complex ( kind = 8 ) CB, the value B.
!
!    Output, real ( kind = 8 ) C, the cosine of the 
!    Givens rotation.
!
!    Output, complex ( kind = 8 ) S, the sine of the 
!    Givens rotation.
!
  implicit none

  complex ( kind = 8 ) alpha
  real ( kind = 8 ) c
  complex ( kind = 8 ) ca
  complex ( kind = 8 ) cb
  real ( kind = 8 ) norm
  complex ( kind = 8 ) s
  real ( kind = 8 ) scale

  if ( abs ( ca ) == 0.0D+00 ) then

    c = 0.0D+00
    s = cmplx ( 1.0D+00, 0.0D+00, kind = 8 )
    ca = cb

  else

    scale = abs ( ca ) + abs ( cb )
    norm = scale * sqrt ( ( abs ( ca / scale ) )**2 &
                        + ( abs ( cb / scale ) )**2 )
    alpha = ca / abs ( ca )
    c = abs ( ca ) / norm
    s = alpha * conjg ( cb ) / norm
    ca = alpha * norm

  end if

  return
end
subroutine zscal ( n, ca, cx, incx )

!*****************************************************************************80
!
!! ZSCAL scales a vector by a constant.
!
!  Discussion:
!
!    This routine uses double precision complex arithmetic.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 April 2006
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input, complex ( kind = 8 ) CA, the multiplier.
!
!    Input/output, complex ( kind = 8 ) CX(*), the vector to be scaled.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive 
!    entries of CX.
!
  implicit none

  complex ( kind = 8 ) ca
  complex ( kind = 8 ) cx(*)
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nincx

  if ( n <= 0 .or. incx <= 0 ) then
    return
  end if

  if ( incx == 1 ) then

    cx(1:n) = ca * cx(1:n)

  else

    nincx = n * incx

    cx(1:nincx:incx) = ca * cx(1:nincx:incx)

  end if

  return
end
subroutine zswap ( n, cx, incx, cy, incy )

!*****************************************************************************80
!
!! ZSWAP interchanges two vectors.
!
!  Discussion:
!
!    This routine uses double precision complex arithmetic.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    28 March 2006
!
!  Author:
!
!    FORTRAN90 version by John Burkardt
!
!  Reference:
!
!    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
!    LINPACK User's Guide,
!    SIAM, 1979,
!    ISBN13: 978-0-898711-72-1,
!    LC: QA214.L56.
!
!    Charles Lawson, Richard Hanson, David Kincaid, Fred Krogh,
!    Basic Linear Algebra Subprograms for Fortran Usage,
!    Algorithm 539,
!    ACM Transactions on Mathematical Software,
!    Volume 5, Number 3, September 1979, pages 308-323.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vectors.
!
!    Input/output, complex ( kind = 8 ) CX(*), one of the vectors to swap.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive 
!    entries of CX.
!
!    Input/output, complex ( kind = 8 ) CY(*), one of the vectors to swap.
!
!    Input, integer ( kind = 4 ) INCY, the increment between successive 
!    elements of CY.
!
  implicit none

  complex ( kind = 8 ) ctemp
  complex ( kind = 8 ) cx(*)
  complex ( kind = 8 ) cy(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) n

  if ( n <= 0 ) then
    return
  end if

  if ( incx == 1 .and. incy == 1 ) then

    do i = 1, n
      ctemp = cx(i)
      cx(i) = cy(i)
      cy(i) = ctemp
    end do

  else

    if ( 0 <= incx ) then
      ix = 1
    else
      ix = ( -n + 1 ) * incx + 1
    end if

    if ( 0 <= incy ) then
      iy = 1
    else
      iy = ( -n + 1 ) * incy + 1
    end if

    do i = 1, n
      ctemp = cx(ix)
      cx(ix) = cy(iy)
      cy(iy) = ctemp
      ix = ix + incx
      iy = iy + incy
    end do

  end if

  return
end
