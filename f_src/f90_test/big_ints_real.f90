program main

!*****************************************************************************80
!
!! MAIN is the main program for BIG_INTS_REAL.
!
!  Discussion:
!
!    It's easy to forget that COMPUTER NUMBERS are not MATHEMATICAL NUMBERS.
!
!    There is a biggest real number.
!
!    There is a biggest integer.
!
!    There is a biggest integer that you can store as a real number.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 September 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BIG_INTS_REAL:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Storing big integers in real variables.'

  call test01 ( )
  call test02 ( )
  call test03 ( )
  call test04 ( )
  call test05 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'BIG_INTS_REAL:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 stores huge integers as reals.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 September 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i4
  integer ( kind = 4 ) i4r4i4
  integer ( kind = 4 ) i4r8i4
  integer ( kind = 8 ) i8
  integer ( kind = 8 ) i8r4i8
  integer ( kind = 8 ) i8r8i8
  real ( kind = 4 ) r4
  real ( kind = 4 ) r4i4
  real ( kind = 4 ) r4i8
  real ( kind = 8 ) r8
  real ( kind = 8 ) r8i4
  real ( kind = 8 ) r8i8

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Compute the largest possible integers.'
  write ( *, '(a)' ) '  Try to store them as real values.'
  write ( *, '(a)' ) '  Then copy them back.'

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  "Huge" integers and huge reals:'
  write ( *, '(a)' ) ''

  i4 = huge ( i4 )
  i8 = huge ( i8 )
  r4 = huge ( r4 )
  r8 = huge ( r8 )

  write ( *, '(a,i26)'   ) '  i4 = huge ( integer ( kind = 4 ) ) = ', i4
  write ( *, '(a,i26)'   ) '  i8 = huge ( integer ( kind = 8 ) ) = ', i8
  write ( *, '(a,g26.6)' ) '  r4 = huge ( real ( kind = 4 ) ) =    ', r4
  write ( *, '(a,g26.6)' ) '  r8 = huge ( real ( kind = 8 ) ) =    ', r8

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Convert huge integers to real values:'
  write ( *, '(a)' ) ''

  r4i4 = real ( i4, kind = 4 )
  r4i8 = real ( i8, kind = 4 )
  r8i4 = real ( i4, kind = 8 )
  r8i8 = real ( i8, kind = 8 )

  write ( *, '(a,g26.6)' ) '  r4i4 = real ( i4, kind = 4 ) =     ', r4i4
  write ( *, '(a,g26.6)' ) '  r4i8 = real ( i8, kind = 4 ) =     ', r4i8
  write ( *, '(a,g26.6)' ) '  r8i4 = real ( i4, kind = 8 ) =     ', r8i4
  write ( *, '(a,g26.6)' ) '  r8i8 = real ( i8, kind = 8 ) =     ', r8i8

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Convert real values of integers back to integers:'
  write ( *, '(a)' ) ''

  i4r4i4 = int ( r4i4, kind = 4 )
  i4r8i4 = int ( r8i4, kind = 4 )
  i8r4i8 = int ( r4i8, kind = 8 )
  i8r8i8 = int ( r8i8, kind = 8 )

  write ( *, '(a,i26)' )   '  i4r4i4 = int ( r4i4, kind = 4 ) =  ', i4r4i4
  write ( *, '(a,i26)' )   '  i4r8i4 = int ( r8i4, kind = 4 ) =  ', i4r8i4
  write ( *, '(a,i26)' )   '  i8r4i8 = int ( r4i8, kind = 8 ) =  ', i8r4i8
  write ( *, '(a,i26)' )   '  i8r8i8 = int ( r8i8, kind = 8 ) =  ', i8r8i8

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02 compares I and int(real(I)).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 September 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i4
  integer ( kind = 4 ) i4_huge
  integer ( kind = 4 ) i4r4i4
  real ( kind = 4 ) r4i4

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  Find the least nonnegative integer I'
  write ( *, '(a)' ) '  for which int(real(I)) /= I'
  write ( *, '(a)' ) '  We use integer ( kind = 4 ) and real ( kind = 4 )'
  write ( *, '(a)' ) '  arithmetic.'

  i4_huge = huge ( i4 )

  i4 = 0
  r4i4 = real ( i4, kind = 4 )
  i4r4i4 = int ( r4i4, kind = 4 )

  do

    if ( i4r4i4 /= i4 ) then
      write ( *, '(a)' ) ''
      write ( *, '(a,i26)'   ) '  I4 =     ', i4
      write ( *, '(a,f27.0)' ) '  R4I4 =   ', r4i4
      write ( *, '(a,i26)'   ) '  I4R4I4 = ', i4r4i4
      exit
    end if

    if ( i4 == i4_huge ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  I4 reached the maximum value.'
      write ( *, '(a)' ) '  No violations were found!'
      exit
    end if

    i4 = i4 + 1
    r4i4 = real ( i4, kind = 4 )
    i4r4i4 = int ( r4i4, kind = 4 )

  end do

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03 compares I and int(real(I)).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    27 September 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i4
  integer ( kind = 4 ) i4_huge
  integer ( kind = 4 ) i4r8i4
  real ( kind = 8 ) r8i4

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  Find the least nonnegative integer I4'
  write ( *, '(a)' ) '  for which int(real(I4)) /= I4'
  write ( *, '(a)' ) '  We use integer ( kind = 4 ) and real ( kind = 8 )'
  write ( *, '(a)' ) '  arithmetic.'

  i4_huge = huge ( i4 )

  i4 = 0
  r8i4 = real ( i4, kind = 8 )
  i4r8i4 = int ( r8i4, kind = 4 )

  do

    if ( i4r8i4 /= i4 ) then
      write ( *, '(a)' ) ''
      write ( *, '(a,i26)'   ) '  I4 =     ', i4
      write ( *, '(a,f27.0)' ) '  R8I4 =   ', r8i4
      write ( *, '(a,i26)'   ) '  I4R8I4 = ', i4r8i4
      exit
    end if

    if ( i4 == i4_huge ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  I4 reached the maximum value.'
      write ( *, '(a)' ) '  No violations were found!'
      exit
    end if

    i4 = i4 + 1
    r8i4 = real ( i4, kind = 8 )
    i4r8i4 = int ( r8i4, kind = 4 )

  end do

  return
end
subroutine test04 ( )

!*****************************************************************************80
!
!! TEST04 tests whether real ( I + 1 ) - real ( I ) = 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 September 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 4 ) a
  real ( kind = 4 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_huge
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST04'
  write ( *, '(a)' ) '  Let I and J be consecutive integer ( kind = 4 ) values.'
  write ( *, '(a)' ) '  Let A and B be real ( kind = 4 ) copies of I and J.'
  write ( *, '(a)' ) '  Seek I and J for which ( B - A ) is not 1.'

  i4_huge = huge ( j )

  j = 0
  b = real ( j, kind = 4 )

  do

    if ( j == i4_huge ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Reached the maximum integer value.'
      write ( *, '(a)' ) '  No violations were found!'
      exit
    end if

    i = j
    j = j + 1
    a = b
    b = real ( j, kind = 4 )

    if ( int ( b - a, kind = 4 ) /= 1 ) then
      write ( *, '(a)' ) ''
      write ( *, * ) '  i =         ', i
      write ( *, * ) '  j = i + 1 = ', j
      write ( *, * ) '  j - i =     ', j - i
      write ( *, * ) '  a = real ( i, kind = 4 ) = ', a
      write ( *, * ) '  b = real ( j, kind = 4 ) = ', b
      write ( *, * ) '  b - a                    = ', b - a
      exit
    end if

  end do

  return
end
subroutine test05 ( )

!*****************************************************************************80
!
!! TEST05 tests whether real ( I + 1 ) - real ( I ) = 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 September 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i4_huge
  integer ( kind = 4 ) j

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST05'
  write ( *, '(a)' ) '  Let I and J be consecutive integer ( kind = 4 ) values.'
  write ( *, '(a)' ) '  Let A and B be real ( kind = 8 ) copies of I and J.'
  write ( *, '(a)' ) '  Seek I and J for which ( B - A ) is not 1.'

  i4_huge = huge ( j )

  j = 0
  b = real ( j, kind = 8 )

  do

    if ( j == i4_huge ) then
      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Reached the maximum integer value.'
      write ( *, '(a)' ) '  No violations were found!'
      exit
    end if

    i = j
    j = j + 1
    a = b
    b = real ( j, kind = 8 )

    if ( int ( b - a, kind = 4 ) /= 1 ) then
      write ( *, '(a)' ) ''
      write ( *, * ) '  i =         ', i
      write ( *, * ) '  j = i + 1 = ', j
      write ( *, * ) '  j - i =     ', j - i
      write ( *, * ) '  a = real ( i, kind = 8 ) = ', a
      write ( *, * ) '  b = real ( j, kind = 8 ) = ', b
      write ( *, * ) '  b - a                    = ', b - a
      exit
    end if

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
