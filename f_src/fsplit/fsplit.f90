program fsplit

!*****************************************************************************80
!
!! fsplit() breaks a single Fortran file into individual, compilable sections.
!
!  Discussion:
! 
!    fsplit() splits a file containing Fortran functions, programs, or
!    subroutines, in free, not fixed form, into separate files, each named 
!    according to the program unit name, with a filename extension ".f90".
!
!  Usage:
!
!    fsplit < file.f90
!
!    fsplit -v < file.f90 
!      to specify "verbose" operation, which prints messages.
!
!    fsplit -a < file.f90
!      to specify "append" operation, in which case the program appends
!      the name of the function, program, or subroutine to each "end" 
!      statement.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 May 2021
!
!  Author:
!
!    Van Snyder
!
  use ISO_Fortran_ENV, only: E => Error_Unit
  implicit NONE
 
  logical :: Append = .false.  ! Append program unit name to "END" statement
                               ! without it select with -a
  integer :: Done              ! Found "end" or "end *" // words(l)
  character(132) :: Comment = '', IOMsg, LcLine, Line, Name
  integer :: I, IOStat, L
  integer :: IWord             ! Which word
  character(4) :: Ext          ! Extension, from the command line
  logical :: Verbose = .false. ! Command argument contains "-v"
  character(10), parameter :: Words(3) = [ 'function  ', &
                                         & 'program   ', &
                                         & 'subroutine' ]

  i = 1
  do
    call get_command_argument ( i, line )
    if ( line(1:1) /= '-' ) exit
    if ( index(line,'a') /= 0 ) append = .true.
    if ( index(line,'v') /= 0 ) verbose = .true.
    i = i + 1
  end do

  ext = line
  if ( ext == '' ) ext = '.f90'
a:do
    ! Look for "function", "program", or "subroutine" as the first thing
    ! on a line
b:  do
      read ( *, '(a)', end=9 ) line
      lcline = adjustl(lower_case ( line ))
      do iword = 1, size(words)
        l = len_trim(words(iword))
        if ( lcline(1:l) == words(iword)(1:l) ) exit b
      end do
      cycle a
    end do b

    lcline = adjustl(lcline(l+1:))
    i = index(lcline,' (')
    if ( i == 0 ) i = len(trim(lcline)) + 1
    name = lcline(1:i-1)
    open ( 10, file=trim(name)//ext, form='formatted', status='new', &
         & iostat=iostat, iomsg=iomsg )
    if ( iostat /= 0 ) then
      write ( e, '(3a,i0/a/a)' ) 'Unable to open ', trim(name)//'.f90', &
        & ', status =', iostat, trim(iomsg), &
        & 'Searching for next program unit'
      cycle a
    end if

    if ( verbose ) write ( e, '(2a)' ) 'Writing ', trim(trim(name) // ext)

    write ( 10, '(a)' ) '1: ' // trim(line)
!
!  Look for "end" followed by words(iword) or end of line or "!"
!
    do
      read ( *, '(a)', end=9 ) line
      lcline = adjustl(lower_case ( line ))
      done = merge(1,0,lcline(1:3) == 'end')

      if ( done /= 0 ) then
        lcline = adjustl(lcline(4:))
        i = index(lcline,'!')
        if ( i /= 0 ) lcline(i:) = ''
        if ( lcline(1:1) == ' ' ) then
          done = 1
        else if ( lcline(1:l) == trim(words(l)) ) then
          lcline = adjustl(lcline(len_trim(words(l))+1:))
          done = 2
          if ( lcline /= '' ) done = 3 ! Assume the name is correct
        else
          done = 0
        end if
      end if

      if ( done /= 0 ) then

        if ( append ) then
          i = index(line,'!')
          if ( i /= 0 ) then
            comment = line(i:)
            line(i:) = ""
          end if

          select case ( done )
          case ( 1 ) ! Bare END statement
            lcline = trim(line) // " " // trim(words(iword)) // " " // trim(name) // &
              & " " // trim(comment)
          case ( 2 ) ! END "words" statement without name
            lcline = trim(line) // " " // trim(name) // " " // comment
          case ( 3 ) ! END statement with words(1) and name
            lcline = line
          end select
        else
          lcline = 'end'
        end if

        write ( 10, '(a)' ) '2: ' // trim(lcline)
        exit
      end if
      write ( 10, '(a)' ) '3: ' // trim(line)
    end do
    close ( 10 )
  end do a
!
!  Come here upon reaching end of input.
!
9 continue

contains

  function Lower_Case ( C ) result ( LC )
    character(*), intent(in) :: C
    character(len(c)) :: LC
    integer :: I
    do i = 1, len(c)
      lc(i:i) = c(i:i)
      if ( c(i:i) >= 'A' .and. c(i:i) <= 'Z' ) &
        lc(i:i) = achar(iachar(c(i:i)) - iachar('A') + iachar('a'))
    end do
  end function Lower_Case

end program fsplit
