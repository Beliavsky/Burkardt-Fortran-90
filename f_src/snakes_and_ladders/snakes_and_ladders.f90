subroutine get_unit ( iunit )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is a value between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is a value between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) IUNIT, the free unit number.
!
  implicit none

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) iunit
  logical ( kind = 4 ) lopen

  iunit = 0

  do i = 1, 99

    if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then

      inquire ( unit = i, opened = lopen, iostat = ios )

      if ( ios == 0 ) then
        if ( .not. lopen ) then
          iunit = i
          return
        end if
      end if

    end if

  end do

  return
end
subroutine snakes ( a )

!*****************************************************************************80
!
!! SNAKES sets up the Snakes and Ladders matrix.
!
!  Discussion:
!
!    Snakes and Ladders, also known as Chutes and Ladders, is a game
!    played on a 10x10 board of 100 squares.  A player can be said to
!    start at square 0, that is, off the board.  The player repeatedly
!    rolls a die, and advances between 1 and 6 steps accordingly.
!    The game is won when the player reaches square 100.  In some versions,
!    the player must reach 100 by exact die count, forfeiting the move
!    if 100 is exceeded; in others, reaching or exceeding 100 counts as
!    a win.
!
!    Play is complicated by the existence of snakes and ladders.  On
!    landing on a square that begins a snake or ladder, the player is
!    immediately tranported to another square, which will be lower for
!    a snake, or higher for a ladder.
!
!    Typically, several players play, alternating turns.
!
!    Given a vector V(0:100) which is initially all zero except for the
!    first entry, the matrix-vector product A'*V represents the probabilities
!    that a player starting on square 0 will be on any given square after one
!    roll.  Correspondingly, (A')^2*V considers two moves, and so on.  Thus,
!    repeatedly multiplying by A' reveals the probability distribution 
!    associated with the likelihood of occupying any particular square at a 
!    given turn in the game.  
!
!    There is a single eigenvalue of value 1, whose corresponding eigenvector
!    is all zero except for a final entry of 1, representing a player who
!    has reached square 100.  All other eigenvalues have norm less than 1,
!    corresponding to the fact that there are no other long term steady
!    states or cycles in the game.
!
!    Note that no two descriptions of the Snakes and Ladders board seem to
!    agree.  This is the board described by Nick Berry.  The board described 
!    by Higham and Higham is close to this one, but differs in the description 
!    of two of the jumps.
!
!    While most commentators elect to move immediately from a snake mouth or
!    ladder foot, I have decide there are reasons to treat the game in such a
!    way that when you land on a ladder foot or snake mouth, you stay there
!    as though you had landed on an ordinary square; the difference arises on
!    your next turn, when, instead of rolling a die, you move up the ladder
!    or down the snake.  This allows the player to "register" a stop at the
!    given square, may be suitable for certain applications, and makes for
!    a transition matrix whose structure is more obvious to understand.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    16 September 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Steve Althoen, Larry King, Kenneth Schilling,
!    How long is a game of Snakes and Ladders?,
!    The Mathematical Gazette,
!    Volume 77, Number 478, March 1993, pages 71-76.
!
!    Nick Berry,
!    Mathematical Analysis of Chutes and Ladders,
!    http://www.datagenetics.com/blog/november12011/index.html
!
!    Desmond Higham, Nicholas Higham,
!    MATLAB Guide,
!    SIAM, 2005,
!    ISBN13: 9780898717891.
!
!  Parameters:
!
!    Output, real ( kind = 8 ) A(0:100,0:100), the matrix.
!
  implicit none

  real ( kind = 8 ) a(0:100,0:100)
  integer ( kind = 4 ) d
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jump(0:100)

  do i = 0, 100
    jump(i) = i
  end do

  jump( 1) =  38
  jump( 4) =  14
  jump( 9) =  31
  jump(16) =   6
  jump(21) =  42
  jump(28) =  84
  jump(36) =  44
  jump(48) =  26
  jump(49) =  11
  jump(51) =  67
  jump(56) =  53
  jump(62) =  19
  jump(64) =  60
  jump(71) =  91
  jump(80) = 100
  jump(87) =  24
  jump(93) =  73
  jump(95) =  75
  jump(98) =  78

  a(0:100,0:100) = 0.0D+00
!
!  A(I,J) represents the probablity that a dice roll will take you from
!  square I to square J.
!
!  Starting in square I...
!
  do i = 0, 100
!
!  If I is a snake or ladder, go to the next spot.
!
    if ( i /= jump(i) ) then

      j = jump(i)
      a(i,j) = 1.0D+00
!
!  Otherwise, roll a die
!
    else

      do d = 1, 6
!
!  so theoretically, our new location J will be I + D,
!
        j = i + d
!
!  but if J is greater than 100, move us back to J,
!
        if ( 100 < j ) then
          j = 100
        end if
  
        a(i,j) = a(i,j) + 1.0D+00 / 6.0D+00

      end do

    end if

  end do

  return
end
subroutine spy_ge ( m, n, a, header )

!*****************************************************************************80
!
!! SPY_GE plots a sparsity pattern for a general storage (GE) matrix.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns
!    in the matrix.
!
!    Input, real ( kind = 8 ) A(M,N), the matrix.
!
!    Input, character ( len = * ) HEADER, the name to be used for the
!    title of the plot, and as part of the names of the data, command
!    and plot files.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  character ( len = * ) header
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  character ( len = 6 ) m_s
  character ( len = 6 ) n_s
  integer ( kind = 4 ) nz_num
  character ( len = 255 ) png_filename
!
!  Create data file.
!
  data_filename = trim ( header ) // '_data.txt'
  call get_unit ( data_unit )
  open ( unit = data_unit, file = data_filename, status = 'replace' )
  nz_num = 0
  do j = 1, n
    do i = 1, m
      if ( a(i,j) /= 0.0D+00 ) then
        write ( data_unit, '(2x,i6,2x,i6)' ) j, i
        nz_num = nz_num + 1
      end if
    end do
  end do
  close ( unit = data_unit )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  Created sparsity data file "' // trim ( data_filename ) // '".'
!
!  Create command file.
!
  command_filename = trim ( header ) // '_commands.txt'
  call get_unit ( command_unit )
  open ( unit = command_unit, file = command_filename, status = 'replace' )

  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'unset key'
  write ( command_unit, '(a)' ) 'set term png'

  png_filename = trim ( header ) // '.png'
  write ( command_unit, '(a)' ) 'set output "' // trim ( png_filename ) // '"'
  write ( command_unit, '(a)' ) 'set size ratio -1'
  write ( command_unit, '(a)' ) 'set xlabel "<--- J --->"'
  write ( command_unit, '(a)' ) 'set ylabel "<--- I --->"'
  
  write ( command_unit, '(a,i6,a)' ) &
    'set title "', nz_num, ' nonzeros for ''' // trim ( header ) // '''"'
  write ( command_unit, '(a)' ) 'set timestamp'
  write ( n_s, '(i6)' ) n
  write ( m_s, '(i6)' ) m
  m_s = adjustl ( m_s )
  n_s = adjustl ( n_s )
  write ( command_unit, '(a)' ) &
    'plot [x=1:' // trim ( n_s) // '] [y=' // trim ( m_s ) // ':1] "' // &
    trim ( data_filename ) // '" with points pt 5'

  close ( unit = command_unit )
  write ( *, '(a)' ) &
    '  Created graphics command file "' // trim ( command_filename ) // '".'

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
