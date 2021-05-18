program main

!*****************************************************************************80
!
!! MAIN is the main program for CHAR_ALLOC.
!
!  Discussion:
!
!    CHAR_ALLOC is a FORTRAN90 program that investigates variations in the
!    declarations of a character variable.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 June 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CHAR_ALLOC:'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Investigate some options for declaration of'
  write ( *, '(a)' ) '  CHARACTER variables.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Note that FORTRAN2003 allows the declaration of'
  write ( *, '(a)' ) '  character variables with an allocatable LEN'
  write ( *, '(a)' ) '  but for earlier FORTRAN''s, this is NOT allowed!'

  call test01 ( )
  call test02 ( )
  call test03 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'CHAR_ALLOC:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01: test CHARACTER ( LEN = 1 ), ALLOCATABLE, DIMENSION ( : ) :: CH_VEC
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 June 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer a_big
  integer a_low
  character ( len = 1 ), allocatable, dimension ( : ) :: ch_vec
  integer i
  integer off

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Test the use of explicit LEN but unknown dimension:'
  write ( *, '(a)' ) &
    '  CHARACTER ( LEN = 1 ), ALLOCATABLE, DIMENSION ( : ) ::CH_VEC'

  allocate ( ch_vec ( 52 ) )

  a_big = iachar ( 'A' )
  a_low = iachar ( 'a' )
  i = 1

  do off = 0, 25
    ch_vec(i)(1:1) = achar ( a_big + off )
    i = i + 1
    ch_vec(i)(1:1) = achar ( a_low + off )
    i = i + 1
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  We allocate 52 entries in the string.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Here is the string after use:'
  write ( *, '(a)' ) ' '

  write ( *, '(a,52a1,a)' ) '  "', ch_vec(1:52)(1:1), '"'

  deallocate ( ch_vec )

  return
end
subroutine test02 ( )

!*****************************************************************************80
!
!! TEST02: test CHARACTER ( LEN = 2 ), ALLOCATABLE, DIMENSION ( : ) :: CH_VEC
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 June 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer a_big
  integer a_low
  character ( len = 2 ), allocatable, dimension ( : ) :: ch_vec
  integer i
  integer off

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  Test the use of explicit LEN but unknown dimension:'
  write ( *, '(a)' ) &
    '  CHARACTER ( LEN = 2 ), ALLOCATABLE, DIMENSION ( : ) ::CH_VEC'

  allocate ( ch_vec ( 26 ) )

  a_big = iachar ( 'A' )
  a_low = iachar ( 'a' )
  i = 1

  do off = 0, 25
    ch_vec(i)(1:1) = achar ( a_big + off )
    ch_vec(i)(2:2) = achar ( a_low + off )
    i = i + 1
  end do

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  We allocate 26 entries in the string.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Here is the string after use:'
  write ( *, '(a)' ) ' '

  write ( *, '(a,26a2,a)' ) '  "', ch_vec(1:26)(1:2), '"'

  deallocate ( ch_vec )

  return
end
subroutine test03 ( )

!*****************************************************************************80
!
!! TEST03: test CHARACTER ( LEN = : ), ALLOCATABLE :: STRING
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    19 March 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  character ( len = : ), allocatable :: string

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  Test the use of unknown LEN:'
  write ( *, '(a)' ) &
    '  CHARACTER ( LEN = : ), ALLOCATABLE :: STRING'

  string = 'hot'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  String = "' // string // '"'
  write ( *, '(a,i4)' ) '  Length of string is ', len ( string )

  string = 'Rama-rama-ding-dong!'
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  String = "' // string // '"'
  write ( *, '(a,i4)' ) '  Length of string is ', len ( string )
 
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
!    06 August 2005
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

  character ( len = 8  ) ampm
  integer   ( kind = 4 ) d
  integer   ( kind = 4 ) h
  integer   ( kind = 4 ) m
  integer   ( kind = 4 ) mm
  character ( len = 9  ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer   ( kind = 4 ) n
  integer   ( kind = 4 ) s
  integer   ( kind = 4 ) values(8)
  integer   ( kind = 4 ) y

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
