subroutine ccs_print ( m, n, ncc, icc, ccc, acc, title )

!*****************************************************************************80
!
!! ccs_PRINT prints a sparse matrix in CCS format.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    23 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of rows in the matrix.
!
!    Input, integer ( kind = 4 ) N, the number of columns in the matrix.
!
!    Input, integer ( kind = 4 ) NCC, the number of CCS elements.
!
!    Input, integer ( kind = 4 ) ICC(NCC), the CCS rows.
!
!    Input, integer ( kind = 4 ) CCC(N+1), the compressed CCS columns.
!
!    Input, real ( kind = 8 ) ACC(NCC), the CCS values.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ncc

  real ( kind = 8 ) acc(ncc)
  integer ( kind = 4 ) ccc(n+1)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) icc(ncc)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  integer ( kind = 4 ) m
  character ( len = * ) title

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) '     #     I     J         A'
  write ( *, '(a)' ) '  ----  ----  ----  ----------------'
  write ( *, '(a)' ) ' '

  if ( ccc(1) == 0 ) then

    j = 0
    do k = 1, ncc
      i = icc(k)
      do while ( ccc(j+2) <= k - 1 )
        j = j + 1
      end do
      write ( *, '(2x,i4,2x,i4,2x,i4,2x,g16.8)' ) k - 1, i, j, acc(k)
    end do

  else

    j = 1
    do k = 1, ncc
      i = icc(k)
      do while ( ccc(j+1) <= k )
        j = j + 1
      end do
      write ( *, '(2x,i4,2x,i4,2x,i4,2x,g16.8)' ) k, i, j, acc(k)
    end do

  end if

  return
end
subroutine ccs_to_st ( m, n, ncc, icc, ccc, acc, nst, ist, jst, ast )

!*****************************************************************************80
!
!! ccs_TO_ST converts sparse matrix information from CCS to ST format.
!
!  Discussion:
!
!    Only JST actually needs to be computed.  The other three output 
!    quantities are simply copies.  
!    
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    23 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of rows.
!
!    Input, integer ( kind = 4 ) N, the number of columns.
!
!    Input, integer ( kind = 4 ) NCC, the number of CCS elements.
!
!    Input, integer ( kind = 4 ) ICC(NCC), the CCS rows.
!
!    Input, integer ( kind = 4 ) CCC(N+1), the CCS compressed columns.
!
!    Input, real ( kind = 8 ) ACC(NCC), the CCS values.
!
!    Output, integer ( kind = 4 ) NST, the number of ST elements.
!
!    Output, integer ( kind = 4 ) IST(NST), JST(NST), the ST rows and columns.
!
!    Output, real ( kind = 8 ) AST(NST), the ST values.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ncc

  real ( kind = 8 ) ast(ncc)
  real ( kind = 8 ) acc(ncc)
  integer ( kind = 4 ) ccc(n+1)
  integer ( kind = 4 ) icc(ncc)
  integer ( kind = 4 ) ist(ncc)
  integer ( kind = 4 ) j
  integer ( kind = 4 ) jhi
  integer ( kind = 4 ) jlo
  integer ( kind = 4 ) jst(ncc)
  integer ( kind = 4 ) k
  integer ( kind = 4 ) khi
  integer ( kind = 4 ) klo
  integer ( kind = 4 ) m
  integer ( kind = 4 ) nst

  nst = 0

  if ( ccc(1) == 0 ) then

    jlo = 0
    jhi = n - 1
  
    do j = jlo, jhi

      klo = ccc(j+1)
      khi = ccc(j+2) - 1

      do k = klo, khi

        nst = nst + 1
        ist(nst) = icc(k+1)
        jst(nst) = j
        ast(nst) = acc(k+1)

      end do

    end do

  else

    jlo = 1
    jhi = n
  
    do j = jlo, jhi

      klo = ccc(j)
      khi = ccc(j+1) - 1

      do k = klo, khi

        nst = nst + 1
        ist(nst) = icc(k)
        jst(nst) = j
        ast(nst) = acc(k)

      end do

    end do

  end if

  return
end
subroutine st_print ( m, n, nst, ist, jst, ast, title )

!*****************************************************************************80
!
!! ST_PRINT prints a sparse matrix in ST format.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, the number of rows.
!
!    Input, integer ( kind = 4 ) N, the number of columns.
!
!    Input, integer ( kind = 4 ) NST, the number of ST elements.
!
!    Input, integer ( kind = 4 ) IST(NST), JST(NST), the ST rows and columns.
!
!    Input, real ( kind = 8 ) AST(NST), the ST values.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer ( kind = 4 ) nst

  real ( kind = 8 ) ast(nst)
  integer ( kind = 4 ) ist(nst)
  integer ( kind = 4 ) jst(nst)
  integer ( kind = 4 ) k
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  character ( len = * ) title

  
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) '     #     I     J       A'
  write ( *, '(a)' ) '  ----  ----  ----  --------------'
  write ( *, '(a)' ) ' '

  do k = 1, nst
    write ( *, '(2x,i4,2x,i4,2x,i4,2x,g16.8)' ) k, ist(k), jst(k), ast(k)
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

  write ( *, '(i2.2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
