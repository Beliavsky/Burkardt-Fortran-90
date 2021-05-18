subroutine caxpy ( n, ca, cx, incx, cy, incy )

!*****************************************************************************80
!
!! CAXPY computes a constant times a vector plus a vector.
!
!  Discussion:
!
!    This routine uses single precision complex arithmetic.
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
!    Input, complex ( kind = 4 ) CA, the multiplier of CX.
!
!    Input, complex ( kind = 4 ) CX(*), the first vector.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive 
!    entries of CX.
!
!    Input/output, complex ( kind = 4 ) CY(*), the second vector.
!    On output, CY(*) has been replaced by CY(*) + CA * CX(*).
!
!    Input, integer ( kind = 4 ) INCY, the increment between successive
!    entries of CY.
!
  implicit none

  complex ( kind = 4 ) ca
  real ( kind = 4 ) cabs1
  complex ( kind = 4 ) cx(*)
  complex ( kind = 4 ) cy(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) n

  if ( n <= 0 ) then
    return
  end if

  if ( cabs1 ( ca ) == 0.0E+00 ) then
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
subroutine ccopy ( n, cx, incx, cy, incy )

!*****************************************************************************80
!
!! CCOPY copies a vector X to a vector Y.
!
!  Discussion:
!
!    This routine uses single precision complex arithmetic.
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
!    Input, complex ( kind = 4 ) CX(*), the first vector.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive 
!    entries of CX.
!
!    Output, complex ( kind = 4 ) CY(*), the second vector.
!
!    Input, integer ( kind = 4 ) INCY, the increment between successive 
!    entries of CY.
!
  implicit none

  complex ( kind = 4 ) cx(*)
  complex ( kind = 4 ) cy(*)
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
function cdotc ( n, cx, incx, cy, incy )

!*****************************************************************************80
!
!! CDOTC forms the conjugated dot product of two vectors.
!
!  Discussion:
!
!    This routine uses single precision complex arithmetic.
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
!    Input, complex ( kind = 4 ) CX(*), the first vector.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive 
!    entries in CX.
!
!    Input, complex ( kind = 4 ) CY(*), the second vector.
!
!    Input, integer ( kind = 4 ) INCY, the increment between successive
!    entries in CY.
!
!    Output, complex ( kind = 4 ) CDOTC, the conjugated dot product of
!    the corresponding entries of CX and CY.
!
  implicit none

  complex ( kind = 4 ) cdotc
  complex ( kind = 4 ) ctemp
  complex ( kind = 4 ) cx(*)
  complex ( kind = 4 ) cy(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) n

  ctemp = cmplx ( 0.0E+00, 0.0E+00 )
  cdotc = cmplx ( 0.0E+00, 0.0E+00 )

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

  cdotc = ctemp

  return
end
function cdotu ( n, cx, incx, cy, incy )

!*****************************************************************************80
!
!! CDOTU forms the unconjugated dot product of two vectors.
!
!  Discussion:
!
!    This routine uses single precision complex arithmetic.
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
!    Input, complex ( kind = 4 ) CX(*), the first vector.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries in CX.
!
!    Input, complex ( kind = 4 ) CY(*), the second vector.
!
!    Input, integer ( kind = 4 ) INCY, the increment between successive 
!    entries in CY.
!
!    Output, complex ( kind = 4 ) CDOTU, the unconjugated dot product of
!    the corresponding entries of CX and CY.
!
  implicit none

  complex ( kind = 4 ) cdotu
  complex ( kind = 4 ) ctemp
  complex ( kind = 4 ) cx(*)
  complex ( kind = 4 ) cy(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) n

  ctemp = cmplx ( 0.0E+00, 0.0E+00 )
  cdotu = cmplx ( 0.0E+00, 0.0E+00 )

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

  cdotu = ctemp

  return
end
subroutine crotg ( ca, cb, c, s )

!*****************************************************************************80
!
!! CROTG determines a Givens rotation.
!
!  Discussion:
!
!    This routine uses single precision complex arithmetic.
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
!    Input/output, complex ( kind = 4 ) CA; on input, the value A.  On output,
!    the value R.
!
!    Input, complex ( kind = 4 ) CB, the value B.
!
!    Output, real ( kind = 4 ) C, the cosine of the Givens rotation.
!
!    Output, complex ( kind = 4 ) S, the sine of the Givens rotation.
!
  implicit none

  complex ( kind = 4 ) alpha
  real ( kind = 4 ) c
  complex ( kind = 4 ) ca
  complex ( kind = 4 ) cb
  real ( kind = 4 ) norm
  complex ( kind = 4 ) s
  real ( kind = 4 ) scale

  if ( abs ( ca ) == 0.0E+00 ) then

    c = 0.0E+00
    s = cmplx ( 1.0E+00, 0.0E+00 )
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
subroutine cscal ( n, ca, cx, incx )

!*****************************************************************************80
!
!! CSCAL scales a vector by a constant.
!
!  Discussion:
!
!    This routine uses single precision complex arithmetic.
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
!    Input, complex ( kind = 4 ) CA, the multiplier.
!
!    Input/output, complex ( kind = 4 ) CX(*), the vector to be scaled.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive 
!    entries of CX.
!
  implicit none

  complex ( kind = 4 ) ca
  complex ( kind = 4 ) cx(*)
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
subroutine csrot ( n, cx, incx, cy, incy, c, s )

!*****************************************************************************80
!
!! CSROT applies a real plane rotation to single precision complex data.
!
!  Discussion:
!
!    The cosine C and sine S are single precision real, while
!    the vectors CX and CY are single precision complex.
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
!    Input/output, complex ( kind = 4 ) CX(*), one of the vectors to be rotated.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive entries
!    of CX.
!
!    Input/output, complex ( kind = 4 ) CY(*), one of the vectors to be rotated.
!
!    Input, integer ( kind = 4 ) INCY, the increment between successive
!    elements of CY.
!
!    Input, real ( kind = 4 ) C, S, parameters (presumably the cosine and sine
!    of some angle) that define a plane rotation.
!
  implicit none

  real ( kind = 4 ) c
  complex ( kind = 4 ) ctemp
  complex ( kind = 4 ) cx(*)
  complex ( kind = 4 ) cy(*)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) incy
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) iy
  integer ( kind = 4 ) n
  real ( kind = 4 ) s

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
subroutine csscal ( n, sa, cx, incx )

!*****************************************************************************80
!
!! CSSCAL scales a vector by a real constant.
!
!  Discussion:
!
!    This routine uses single precision complex arithmetic.
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
!    Input, real ( kind = 4 ) SA, the multiplier.
!
!    Input/output, complex ( kind = 4 ) CX(*), the vector to be scaled.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of the vector CX.
!
  implicit none

  complex ( kind = 4 ) cx(*)
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nincx
  real ( kind = 4 ) sa

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
subroutine cswap ( n, cx, incx, cy, incy )

!*****************************************************************************80
!
!! CSWAP interchanges two vectors.
!
!  Discussion:
!
!    This routine uses single precision complex arithmetic.
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
!    Input/output, complex ( kind = 4 ) CX(*), one of the vectors to swap.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of CX.
!
!    Input/output, complex ( kind = 4 ) CY(*), one of the vectors to swap.
!
!    Input, integer ( kind = 4 ) INCY, the increment between successive
!    elements of CY.
!
  implicit none

  complex ( kind = 4 ) ctemp
  complex ( kind = 4 ) cx(*)
  complex ( kind = 4 ) cy(*)
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

    do i = 1,n
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
function icamax ( n, x, incx )

!*****************************************************************************80
!
!! ICAMAX indexes the vector element of maximum absolute value.
!
!  Discussion:
!
!    This routine uses single precision complex arithmetic.
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
!    Input, complex ( kind = 4 ) X(*), the vector.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of X.
!
!    Output, integer ( kind = 4 ) ICAMAX, the index of the element of maximum
!    absolute value.
!
  implicit none

  real ( kind = 4 ) cabs1
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icamax
  integer ( kind = 4 ) incx
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) n
  real ( kind = 4 ) smax
  complex ( kind = 4 ) x(*)

  icamax = 0
  if ( n < 1 .or. incx  <=  0 ) then
    return
  end if

  icamax = 1

  if ( n == 1 ) then
    return
  end if

  if ( incx /= 1 ) then

    ix = 1
    smax = cabs1 ( x(1) )
    ix = ix + incx
    do i = 2, n
      if ( smax < cabs1 ( x(ix) ) ) then
        icamax = i
        smax = cabs1 ( x(ix) )
      end if
      ix = ix + incx
    end do

  else

    smax = cabs1 ( x(1) )
    do i = 2, n
      if ( smax < cabs1 ( x(i) ) ) then
        icamax = i
        smax = cabs1 ( x(i) )
      end if
    end do

  end if

  return
end
function scasum ( n, x, incx )

!*****************************************************************************80
!
!! SCASUM takes the sum of the absolute values of a vector.
!
!  Discussion:
!
!    This routine uses single precision complex arithmetic.
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
!    Input, complex ( kind = 4 ) X(*), the vector.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of X.
!
!    Output, real ( kind = 4 ) SCASUM, the sum of the absolute values.
!
  implicit none

  integer ( kind = 4 ) incx
  integer ( kind = 4 ) n
  integer ( kind = 4 ) nincx
  real ( kind = 4 ) scasum
  complex ( kind = 4 ) x(*)

  scasum = 0.0E+00

  if ( n <= 0 .or. incx <= 0 ) then
    return
  end if

  if ( incx == 1 ) then

    scasum = sum ( abs ( real ( x(1:n) ) ) + abs ( aimag ( x(1:n) ) ) )

  else

    nincx = n * incx

    scasum = sum ( abs ( real ( x(1:nincx:incx) ) ) &
                 + abs ( aimag ( x(1:nincx:incx) ) ) )

  end if

  return
end
function scnrm2 ( n, x, incx )

!*****************************************************************************80
!
!! SCNRM2 returns the euclidean norm of a vector.
!
!  Discussion:
!
!    This routine uses single precision complex arithmetic.
!
!    SCNRM2 := sqrt ( sum ( conjg ( x(1:n) ) * x(1:n) ) )
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
!    Input, complex ( kind = 4 ) X(*), the vector.
!
!    Input, integer ( kind = 4 ) INCX, the increment between successive
!    entries of X.
!
!    Output, real ( kind = 4 ) SCNRM2, the norm of the vector.
!
  implicit none

  integer ( kind = 4 ) incx
  integer ( kind = 4 ) ix
  integer ( kind = 4 ) n
  real ( kind = 4 ) norm
  real ( kind = 4 ) scale
  real ( kind = 4 ) scnrm2
  real ( kind = 4 ) ssq
  real ( kind = 4 ) temp
  complex ( kind = 4 ) x(*)

  if ( n < 1 .or. incx < 1 ) then

    norm = 0.0E+00

  else

    scale = 0.0E+00
    ssq = 1.0E+00

    do ix = 1, 1 + ( n - 1 ) * incx, incx

      if ( real ( x(ix) ) /= 0.0E+00 ) then
        temp = abs ( real ( x(ix) ) )
        if ( scale < temp ) then
          ssq = 1.0E+00 + ssq * ( scale / temp )**2
          scale = temp
        else
          ssq = ssq + ( temp / scale )**2
        end if
      end if

      if ( aimag ( x(ix) ) /= 0.0E+00 ) then
        temp = abs ( aimag ( x(ix) ) )
        if ( scale < temp ) then
          ssq = 1.0E+00 + ssq * ( scale / temp )**2
          scale = temp
        else
          ssq = ssq + ( temp / scale )**2
        end if

      end if

    end do

    norm  = scale * sqrt ( ssq )

  end if

  scnrm2 = norm

  return
end
