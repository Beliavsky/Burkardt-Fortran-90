subroutine r8vec_transpose_print ( n, a, title )

!*****************************************************************************80
!
!! R8VEC_TRANSPOSE_PRINT prints an R8VEC "transposed".
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Example:
!
!    A = (/ 1.0, 2.1, 3.2, 4.3, 5.4, 6.5, 7.6, 8.7, 9.8, 10.9, 11.0 /)
!    TITLE = 'My vector:  '
!
!    My vector:  1.0    2.1    3.2    4.3    5.4
!                6.5    7.6    8.7    9.8   10.9
!               11.0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 August 2016
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of components of the vector.
!
!    Input, real ( kind = 8 ) A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) a(n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ihi
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ) j
  character ( len = * ) title
  integer ( kind = 4 ) title_length

  title_length = len_trim ( title )

  do ilo = 1, n, 5
    if ( ilo == 1 ) then
      write ( *, '(a)', advance = 'NO' ) trim ( title )
    else
      do i = 1, title_length
        write ( *, '(1x)', advance = 'NO' )
      end do
    end if
    write ( *, '(2x)', advance = 'NO' )
    ihi = min ( ilo + 5 - 1, n )
    do j = ilo, ihi
      write ( *, '(g14.6)', advance = 'NO' ) a(j)
    end do
    write ( *, '(a)' )

  end do
 
  return
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2013
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

  character ( len = 8 ) ampm
  integer ( kind = 4 ) d
  integer ( kind = 4 ) h
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s
  integer ( kind = 4 ) values(8)
  integer ( kind = 4 ) y

  call date_and_time ( values = values )

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

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
function vdc_base ( i, b )

!*****************************************************************************80
!
!! VDC_BASE computes an element of the van der Corput sequence in any base.
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
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the index of the element of the sequence.
!    I = 0 is allowed, and returns R = 0.
!
!    Input, integer ( kind = 4 ) B, the base of the sequence.  The standard 
!    sequence uses B = 2, and this function expects 2 <= B.
!
!    Output, real ( kind = 8 ) VDC_BASE, the I-th element of the van der 
!    Corput sequence.
!
  implicit none

  integer ( kind = 4 ) b
  real ( kind = 8 ) base_inv
  integer ( kind = 4 ) digit
  integer ( kind = 4 ) i
  real ( kind = 8 ) r
  real ( kind = 8 ) s
  integer ( kind = 4 ) t
  real ( kind = 8 ) vdc_base
!
!  2 <= B.
!
  if ( b < 2 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'VDC_BASE - Fatal error!'
    write ( *, '(a)' ) '  2 <= B is required.'
    stop 1
  end if
!
!  Isolate the sign.
!
  if ( i < 0 ) then
    s = -1.0D+00
  else
    s = +1.0D+00
  end if
!
!  Only work with the nonnegative part of I.
!
  t = abs ( i )
!
!  Carry out the computation.
!
  base_inv = 1.0D+00 / real ( b, kind = 8 )

  r = 0.0D+00

  do while ( t /= 0 )
    digit = mod ( t, b )
    r = r + real ( digit, kind = 8 ) * base_inv
    base_inv = base_inv / b
    t = ( t / b )
  end do
!
!  Recover the sign.
!
  r = r * s

  vdc_base = r

  return
end
function vdc_inverse ( r )

!*****************************************************************************80
!
!! VDC_INVERSE inverts an element of the van der Corput sequence.
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
!  Parameters:
!
!    Input, real ( kind = 8 ) R, the I-th element of the van der Corput 
!    sequence.  |R| < 1.0
!
!    Output, integer ( kind = 4 ) VDC_INVERSE, the index of the element of 
!    the sequence.
!
  implicit none

  integer ( kind = 4 ) d
  integer ( kind = 4 ) i
  integer ( kind = 4 ) p
  real ( kind = 8 ) r
  integer ( kind = 4 ) s
  real ( kind = 8 ) t
  integer ( kind = 4 ) vdc_inverse

  if ( r < 0.0D+00 ) then
    s = -1
  else
    s = +1
  end if

  if ( 1.0 <= abs ( r ) ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'VDC_INVERSE - Fatal error!'
    write ( *, '(a)' ) '  |R| < 1.0 is required.'
    stop 1
  end if

  t = abs ( r )

  i = 0
  p = 1

  do while ( t /= 0.0D+00 )
    t = t * 2.0D+00
    d = int ( t )
    i = i + d * p
    p = p * 2
    t = mod ( t, 1.0D+00 )
  end do
!
!  Recover the sign.
!
  i = i * s

  vdc_inverse = i

  return
end
function vdc ( i )

!*****************************************************************************80
!
!! VDC computes an element of the van der Corput sequence.
!
!  Discussion:
!
!    The van der Corput sequence is often used to generate a "subrandom"
!    sequence of points which have a better covering property
!    than pseudorandom points.
!
!    The van der Corput sequence generates a sequence of points in [0,1]
!    which never repeats.  The elements of the van der Corput sequence 
!    are strictly less than 1.
!
!    The van der Corput sequence writes an integer in a given base 2,
!    and then its digits are "reflected" about the decimal point.
!    This maps the numbers from 1 to N into a set of numbers in [0,1],
!    which are especially nicely distributed if N is one less
!    than a power of the base.
!
!    The generation is quite simple.  Given an integer I, the expansion
!    of I in base 2 is generated.  Then, essentially, the result R
!    is generated by writing a decimal point followed by the digits of
!    the expansion of I, in reverse order.  This decimal value is actually
!    still in base 2, so it must be properly interpreted to generate
!    a usable value.
!
!  Example:
!
!    I        I         van der Corput
!    decimal  binary    binary   decimal
!    -------  ------    ------   -------
!        0  =     0  =>  .0     = 0.0
!        1  =     1  =>  .1     = 0.5
!        2  =    10  =>  .01    = 0.25
!        3  =    11  =>  .11    = 0.75
!        4  =   100  =>  .001   = 0.125
!        5  =   101  =>  .101   = 0.625
!        6  =   110  =>  .011   = 0.375
!        7  =   111  =>  .111   = 0.875
!        8  =  1000  =>  .0001  = 0.0625
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
!  Reference:
!
!    John Halton,
!    On the efficiency of certain quasi-random sequences of points
!    in evaluating multi-dimensional integrals,
!    Numerische Mathematik,
!    Volume 2, pages 84-90, 1960.
!
!    John Hammersley,
!    Monte Carlo methods for solving multivariable problems,
!    Proceedings of the New York Academy of Science,
!    Volume 86, pages 844-874, 1960.
!
!    Johannes van der Corput,
!    Verteilungsfunktionen I & II,
!    Nederl. Akad. Wetensch. Proc.,
!    Volume 38, 1935, pages 813-820, pages 1058-1066.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) I, the index of the element of the sequence.
!    I = 0 is allowed, and returns R = 0.
!
!    Output, real ( kind = 8 ) VDC, the I-th element of the van der Corput 
!    sequence.
!
  implicit none

  real ( kind = 8 ) base_inv
  integer ( kind = 4 ) d
  integer ( kind = 4 ) i
  real ( kind = 8 ) r
  real ( kind = 8 ) s
  integer ( kind = 4 ) t
  real ( kind = 8 ) vdc
!
!  Isolate the sign.
!
  if ( i < 0 ) then
    s = -1.0D+00
  else
    s = +1.0D+00
  end if
!
!  Work with the magnitude of I.
!
  t = abs ( i )
!
!  Carry out the computation.
!
  base_inv = 0.5D+00

  r = 0.0D+00

  do while ( t /= 0 )
    d = mod ( t, 2 )
    r = r + real ( d, kind = 8 ) * base_inv
    base_inv = base_inv / 2.0D+00
    t = ( t / 2 )
  end do
!
!  Recover the sign.
!
  r = r * s

  vdc = r

  return
end
subroutine vdc_sequence ( i1, i2, r )

!*****************************************************************************80
!
!! VDC_SEQUENCE computes a sequence of elements of the van der Corput sequence.
!
!  Discussion:
!
!    This function could be rewritten to take advantage of MATLAB's
!    vectorization capabilities.
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
!  Parameters:
!
!    Input, integer ( kind = 4 ) I1, I2, the indices of the first and
!    last elements.
!
!    Output, real ( kind = 8 ) R(|I2+1-I1|), elements I1 through I2 of 
!    the van der Corput sequence.
!
  implicit none

  integer ( kind = 4 ) i1
  integer ( kind = 4 ) i2

  real ( kind = 8 ) base_inv
  integer ( kind = 4 ) d
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i3
  integer ( kind = 4 ) j
  real ( kind = 8 ) r(*)
  real ( kind = 8 ) s
  integer ( kind = 4 ) t

  if ( i1 <= i2 ) then
    i3 = +1
  else
    i3 = -1
  end if

  j = 0

  do i = i1, i2, i3

    j = j + 1
!
!  Isolate the sign.
!
    if ( i < 0 ) then
      s = -1.0D+00
    else
      s = +1.0D+00
    end if
!
!  Work with the magnitude of I.
!
    t = abs ( i )
!
!  Carry out the computation.
!
    base_inv = 0.5D+00

    r(j) = 0.0D+00

    do while ( t /= 0 )
      d = mod ( t, 2 )
      r(j) = r(j) + real ( d, kind = 8 ) * base_inv
      base_inv = base_inv / 2.0D+00
      t = ( t / 2 )
    end do
!
!  Recover the sign.
!
    r(j) = r(j) * s

  end do

  return
end

