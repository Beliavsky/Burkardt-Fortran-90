subroutine i4mat_to_i4vec ( m, n, a, b )

!*****************************************************************************80
!
!! I4MAT_TO_I4VEC copies an I4MAT into an I4VEC.
!
!  Discussion:
!
!    An I4MAT is an M by N array of I4's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 April 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, integer ( kind = 4 ) A(M,N), an M by N matrix.
!
!    Output, integer ( kind = 4 ) B(M*N), the vector.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) b(m*n)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  do j = 1, n
    do i = 1, m
      b(j+(i-1)*n)= a(i,j)
    end do
  end do

  return
end
subroutine i4mat_flip_cols ( m, n, a )

!*****************************************************************************80
!
!! I4MAT_FLIP_COLS swaps the columns of an I4MAT.
!
!  Discussion:
!
!    An I4MAT is an M by N array of I4's.
!
!    To "flip" the columns of an I4MAT is to start with something like
!
!      11 12 13 14 15
!      21 22 23 24 25
!      31 32 33 34 35
!      41 42 43 44 45
!      51 52 53 54 55
!
!    and return
!
!      15 14 13 12 11
!      25 24 23 22 21
!      35 34 33 32 31
!      45 44 43 42 41
!      55 54 53 52 51
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 June 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input/output, integer ( kind = 4 ) A(M,N), the matrix whose columns
!    are to be flipped.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) b(m)
  integer ( kind = 4 ) j

  do j = 1, n / 2
    b(1:m      ) = a(1:m,    j)
    a(1:m,    j) = a(1:m,n+1-j)
    a(1:m,n+1-j) = b(1:m)
  end do

  return
end
subroutine i4mat_flip_rows ( m, n, a )

!*****************************************************************************80
!
!! I4MAT_FLIP_ROWS swaps the rows of an I4MAT.
!
!  Discussion:
!
!    An I4MAT is an M by N array of I4's.
!
!    To "flip" the rows of an I4MAT is to start with something like
!
!      11 12 13 14 15
!      21 22 23 24 25
!      31 32 33 34 35
!      41 42 43 44 45
!      51 52 53 54 55
!
!    and return
!
!      51 52 53 54 55
!      41 42 43 44 45
!      31 32 33 34 35
!      21 22 23 24 25
!      11 12 13 14 15
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 June 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input/output, integer ( kind = 4 ) A(M,N), the matrix whose rows
!    are to be flipped.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) b(n)
  integer ( kind = 4 ) i

  do i = 1, m / 2
    b(      1:n) = a(    i,1:n)
    a(    i,1:n) = a(m+1-i,1:n)
    a(m+1-i,1:n) = b(      1:n)
  end do

  return
end
subroutine i4mat_transpose ( m, n, a )

!*****************************************************************************80
!
!! I4MAT_TRANSPOSE transposes an I4MAT.
!
!  Discussion:
!
!    An I4MAT is an M by N array of I4's.
!
!    GNU Fortran's "transpose()" function doesn't do the job here.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 April 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns.
!
!    Input, integer ( kind = 4 ) A(M,N), an M by N matrix.
!
!    Output, integer ( kind = 4 ) A(N,M), the transposed matrix.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m*n)
  integer ( kind = 4 ), allocatable :: b(:)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j

  allocate ( b(m*n) )

  do j = 1, n
    do i = 1, m
      b(j+(i-1)*n)= a(i+(j-1)*m)
    end do
  end do

  do j = 1, n
    do i = 1, m
      a(j+(i-1)*n) = b(j+(i-1)*n)
    end do
  end do
  
  deallocate ( b )

  return
end
subroutine pentomino_matrix ( name, p_m, p_n, p )

!*****************************************************************************80
!
!! PENTOMINO_MATRIX returns a 0/1 matrix defining a particular pentomino.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( length = 1 ) NAME, is 'f', 'i', 'l', 'n', 'p', 't', 
!    'u', 'v', 'w', 'x', 'y' or 'z'.
!
!    Output, integer ( kind = 4 ) P_M, P_N, the number of rows and columns of 
!    the representation.
!
!    Output, integer ( kind = 4 ) P(P_Mp_N), a P_MxP_N matrix of 0's and 1's 
!    that indicates the shape of the pentomino.
!
  implicit none

  character ( len = 1 ) name
  integer ( kind = 4 ) p(5*5)
  integer ( kind = 4 ) p_m
  integer ( kind = 4 ) p_n

  integer ( kind = 4 ), save :: f_mat(3*3) = (/ &
     0, 1, 0, &
     1, 1, 1, &
     1, 0, 0 /)
  integer ( kind = 4 ), save :: i_mat(5*1) = (/ &
    1, 1, 1, 1, 1 /)
  integer ( kind = 4 ), save :: l_mat(4*2) = (/ &
    1, 1, 1, 1, &
    0, 0, 0, 1 /)
  integer ( kind = 4 ), save :: n_mat(2*4) = (/ &
    1, 0, &
    1, 1, &
    0, 1, &
    0, 1 /)
  integer ( kind = 4 ), save :: p_mat(3*2) = (/ &
    1, 1, 1, &
    1, 1, 0 /)
  integer ( kind = 4 ), save :: t_mat(3*3) = (/ &
    1, 0, 0, &
    1, 1, 1, &
    1, 0, 0 /)
  integer ( kind = 4 ), save :: u_mat(2*3) = (/ &
    1, 1, &
    0, 1, &
    1, 1 /)
  integer ( kind = 4 ), save :: v_mat(3*3) = (/ &
    1, 1, 1, &
    0, 0, 1, &
    0, 0, 1 /)
  integer ( kind = 4 ), save :: w_mat(3*3) = (/ &
    1, 1, 0, &
    0, 1, 1, &
    0, 0, 1 /)
  integer ( kind = 4 ), save :: x_mat(3*3) = (/ &
    0, 1, 0, &
    1, 1, 1, &
    0, 1, 0 /)
  integer ( kind = 4 ), save :: y_mat(2*4) = (/ &
    0, 1, &
    0, 1, &
    1, 1, &
    0, 1 /)
  integer ( kind = 4 ), save :: z_mat(3*3) = (/ &
    1, 0, 0, &
    1, 1, 1, &
    0, 0, 1 /)

  p(1:5*5) = 0

  if ( name == 'f' .or. name == 'F' ) then
    p_m = 3
    p_n = 3
    p(1:p_m*p_n) = f_mat(1:p_m*p_n)
  else if ( name == 'i' .or. name == 'I' ) then
    p_m = 5
    p_n = 1
    p(1:p_m*p_n) = i_mat(1:p_m*p_n)
  else if ( name == 'l' .or. name == 'L' ) then
    p_m = 4
    p_n = 2
    p(1:p_m*p_n) = l_mat(1:p_m*p_n)
  else if ( name == 'n' .or. name == 'N' ) then
    p_m = 2
    p_n = 4
    p(1:p_m*p_n) = n_mat(1:p_m*p_n)
  else if ( name == 'p' .or. name == 'P' ) then
    p_m = 3
    p_n = 2
    p(1:p_m*p_n) = p_mat(1:p_m*p_n)
  else if ( name == 't' .or. name == 'T' ) then
    p_m = 3
    p_n = 3
    p(1:p_m*p_n) = t_mat(1:p_m*p_n)
  else if ( name == 'u' .or. name == 'U' ) then
    p_m = 2
    p_n = 3
    p(1:p_m*p_n) = u_mat(1:p_m*p_n)
  else if ( name == 'v' .or. name == 'V' ) then
    p_m = 3
    p_n = 3
    p(1:p_m*p_n) = v_mat(1:p_m*p_n)
  else if ( name == 'w' .or. name == 'W' ) then
    p_m = 3
    p_n = 3
    p(1:p_m*p_n) = w_mat(1:p_m*p_n)
  else if ( name == 'x' .or. name == 'X' ) then
    p_m = 3
    p_n = 3
    p(1:p_m*p_n) = x_mat(1:p_m*p_n)
  else if ( name == 'y' .or. name == 'Y' ) then
    p_m = 2
    p_n = 4
    p(1:p_m*p_n) = y_mat(1:p_m*p_n)
  else if ( name == 'z' .or. name == 'Z' ) then
    p_m = 3
    p_n = 3
    p(1:p_m*p_n) = z_mat(1:p_m*p_n)
  else
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'PENTOMINO_MATRIX - Fatal error!'
    write ( *, '(a)' ) '  Illegal name = "' // name // '"'
    write ( *, '(a)' ) '  Legal names: f, i, l, n, p, t, u, v, w, x, y, z.'
    stop 1
  end if

  return
end
subroutine pentomino_plot ( p_m, p_n, p, label )

!*****************************************************************************80
!
!! PENTOMINO_PLOT plots a particular pentomino in a 5x5 grid.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    22 April 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) P_M, P_N, the number of rows and columns of the
!    representation.
!
!    Input, integer ( kind = 4 ) P(P_M*P_N), a matrix of 0's and 1's.
!    1 <= P_M, P_N <= 5.  There should be exactly 5 values of one.
!
!    Input, string LABEL, a title for the plot.
!
  implicit none

  integer ( kind = 4 ) p_m
  integer ( kind = 4 ) p_n

  character ( len = 16 ) color
  integer ( kind = 4 ) color_index(5,5)
  character ( len = 80 ) command_filename
  integer ( kind = 4 ) command_unit
  integer ( kind = 4 ) i
  integer ( kind = 4 ) i_reverse
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  character ( len = * ) label
  integer ( kind = 4 ) :: m = 5
  integer ( kind = 4 ) :: n = 5
  integer ( kind = 4 ) p(p_m*p_n)
  character ( len = 80 ) plot_filename

  command_filename = trim ( label ) // '_commands.txt'
  plot_filename = trim ( label ) // '.png'
!
!  Initially, the grid is entirely white (color 0)
!
  color_index(1:m,1:n) = 0
!
!  Place the pentomino on the grid, so that it is "snug" in the upper left corner.
!
  do j = 1, p_n
    do i = 1, p_m
      color_index(i,j) = p(i+(j-1)*p_m)
    end do
  end do
!
!  Create the command file.
!
  call get_unit ( command_unit )
  open ( unit = command_unit, file = command_filename, &
    status = 'replace' )
  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // &
    trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) 'set output "' // &
    trim ( plot_filename ) // '"'
  write ( command_unit, '(a)' ) 'set title "' // label // '"'
!
!  Get a plot of TRUE SQUARES.
!
  write ( command_unit, '(a)' ) 'set xrange [ 0 : 5 ]'
  write ( command_unit, '(a)' ) 'set yrange [ 0 : 5 ]'
  write ( command_unit, '(a)' ) 'set size square'
  write ( command_unit, '(a)' ) 'unset border'
  write ( command_unit, '(a)' ) 'unset tics'
  write ( command_unit, '(a)' ) 'set nokey'

  k = 0
  do i = 1, m
    do j = 1, n
      k = k + 1

      if ( color_index(i,j) == 0 ) then
        color = "white"
      else if ( color_index(i,j) == 1 ) then
        color = "black"
      end if

      i_reverse = m + 1 - i

      write ( command_unit, '(a,i4,a,i2,a,i2,a,i2,a,i2,a)' ) &
        'set object ', k, ' rect from ', j-1, ',', i_reverse-1, &
        ' to ', j, ',', i_reverse, ' back'
      write ( command_unit, '(a,i4,a)' ) &
        'set object ', k, ' rect fc rgb "' // trim ( color ) // &
        '" fillstyle solid 1.0'

    end do
  end do
!
!  If you don't have some bogus PLOT command here, all the previous work
!  results in no plot all.  Way to go, gnuplot!
!  Here, we plot the function y = -1, which is out of range and won't show up.
!
  write ( command_unit, '(a)' ) 'plot -1 with lines'
  close ( unit = command_unit )

  write ( *, '(a)' ) &
    '  Created command file "' // trim ( command_filename ) // '".'

  return
end
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
subroutine polyomino_condense ( mp, np, p, mq, nq, q )

!*****************************************************************************80
!
!! POLYOMINO_CONDENSE condenses a polyomino.
!
!  Discussion:
!
!    A polyomino is a shape formed by connecting unit squares edgewise.
!
!    A polyomino can be represented by an MxN matrix, whose entries are
!    1 for squares that are part of the polyomino, and 0 otherwise.
!
!    This program is given an MxN matrix that is meant to represent a 
!    polyomino.  It first replaces all nonzero entries by the value 1.
!    It then "condenses" the matrix, if possible, by removing initial and
!    final rows and columns that are entirely zero.
!
!    While this procedure might save a slight amount of space, its purpose
!    is to simplify the task of manipulating polyominos, embedding them in
!    larger shapes, and detecting whether two polyominos describe the same
!    shape.
!
!    It is entirely possible, and usual, that the output quantities are
!    simply copies of the input quantities.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 April 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) MP, NP, the number of rows and columns in the 
!    representation of the polyomino P.
!
!    Input, integer ( kind = 4 ) P(MP*NP), a matrix of 0's and 1's representing 
!    the polyomino.  
!
!    Output, integer ( kind = 4 ) MQ, NQ, the number of rows and columns of the
!    condensed polyomino.
!
!    Output, integer ( kind = 4 ) Q(MQ*NQ), the representation of the condensed
!    polyomino.
!
  implicit none

  integer ( kind = 4 ) mp
  integer ( kind = 4 ) mq
  integer ( kind = 4 ) np
  integer ( kind = 4 ) nq

  integer ( kind = 4 ) i
  integer ( kind = 4 ) i_max
  integer ( kind = 4 ) i_min
  integer ( kind = 4 ) j
  integer ( kind = 4 ) j_max
  integer ( kind = 4 ) j_min
  integer ( kind = 4 ) p(mp*np)
  integer ( kind = 4 ) q(mq*nq)
!
!  Discard nonsense.
!
  if ( mp <= 0 .or. np <= 0 ) then
    mq = 0
    nq = 0
    return
  end if
!
!  Seek first and last nonzero rows, columns.
!
  i_min = -1
  do i = 1, mp
    do j = 1, np
      if ( p(i+(j-1)*mp) /= 0 ) then
        i_min = i;
        exit
      end if
    end do
    if ( i_min /= -1 ) then
      exit
    end if
  end do
!
!  If I_MIN = -1, then we have a null matrix.
!
  if ( i_min == -1 ) then
    mq = 0
    nq = 0
    return
  end if

  i_max = mp + 1
  do i = mp, 1, -1
    do j = 1, np
      if ( p(i+(j-1)*mp) /= 0 ) then
        i_max = i
        exit
      end if
    end do
    if ( i_max /= mp + 1 ) then
      exit
    end if
  end do

  j_min = -1
  do j = 1, np
    do i = 1, mp
      if ( p(i+(j-1)*mp) /= 0 ) then
        j_min = j
        exit
      end if
    end do
    if ( j_min /= -1 ) then
      exit
    end if
  end do

  j_max = np + 1
  do j = np, 1, -1
    do i = 1, mp
      if ( p(i+(j-1)*mp) /= 0 ) then
        j_max = j
        exit
      end if
    end do
    if ( j_max /= np + 1 ) then
      exit
    end if
  end do
!
!  Measure the nonzero block.
!
  mq = i_max + 1 - i_min
  nq = j_max + 1 - j_min
!
!  Copy the nonzero block.
!
  do j = 1, nq
    do i = 1, mq
      if ( p(i+i_min-1+(j+j_min-1-1)*mp) /= 0 ) then
        q(i+(j-1)*mq) = 1
      else
        q(i+(j-1)*mq) = 0
      end if
    end do
  end do

  return
end
subroutine polyomino_embed_list ( mr, nr, r, mp, np, p, number, list )

!*****************************************************************************80
!
!! POLYOMINO_EMBED_LIST lists the polyomino embeddings in a region.
!
!  Discusion:
!
!    A region R is a subset of an MRxNR grid of squares.
!
!    A polyomino P is a subset of an MPxNP grid of squares.
!
!    Both objects are represented by binary matrices, with the property that
!    there are no initial or final zero rows or columns.
!
!    For this computation, we regard P as a "fixed" polyomino in other words,
!    no reflections or rotations will be allowed.
!
!    An "embedding" of P into R is an offset (MI,NJ) such that 
!      P(I,J) = R(I+MI,J+NJ) 
!      for 1 <= I <= MP, 1 <= J <= NP, and 
!      for 0 <= MI <= MR-MP, 0 <= MJ <= NR-NP.
!    We can detect an embedding simply by taking what amounts to a kind of
!    dot product of P with a corresponding subregion of R.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 May 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) MR, NR, the number of rows and columns in the 
!    representation of the region R.
!
!    Input, integer ( kind = 4 ) R(MR,NR), a matrix of 0's and 1's 
!    representing the region.
!
!    Input, integer ( kind = 4 ) MP, NP, the number of rows and columns in the 
!    representation of the polyomino P.
!
!    Input, integer ( kind = 4 ) P(MP,NP), a matrix of 0's and 1's representing
!    the polyomino.
!
!    Input, integer ( kind = 4 ) NUMBER, the number of embeddings.
!
!    Output, integer ( kind = 4 ) LIST(NUMBER,2), for each embedding, the I and 
!    J offsets applied to the polyomino P.
!
  implicit none

  integer ( kind = 4 ) mp
  integer ( kind = 4 ) mr
  integer ( kind = 4 ) np
  integer ( kind = 4 ) nr
  integer ( kind = 4 ) number

  integer ( kind = 4 ) k
  integer ( kind = 4 ) list(number,2)
  integer ( kind = 4 ) mi
  integer ( kind = 4 ) nj
  integer ( kind = 4 ) p(mp,np)
  integer ( kind = 4 ) pr
  integer ( kind = 4 ) r(mr,nr)
  integer ( kind = 4 ) srp
!
!  Count the 1's in P.
!
  pr = sum ( p(1:mp,1:np) )
!
!  For each possible (I,J) coordinate of the upper left corner of a subset of R,
!  see if it matches P.
!
  k = 0
  do mi = 0, mr - mp
    do nj = 0, nr - np
      srp = sum ( p(1:mp,1:np) * r(1+mi:mp+mi,1+nj:np+nj) )
      if ( srp == pr ) then
        k = k + 1
        list(k,1) = mi
        list(k,2) = nj
      end if
    end do
  end do

  return
end
subroutine polyomino_embed_number ( mr, nr, r, mp, np, p, number )

!*****************************************************************************80
!
!! POLYOMINO_EMBED_NUMBER counts the number of polyomino embeddings in a region.
!
!  Discusion:
!
!    A region R is a subset of an MRxNR grid of squares.
!
!    A polyomino P is a subset of an MPxNP grid of squares.
!
!    Both objects are represented by binary matrices, with the property that
!    there are no initial or final zero rows or columns.
!
!    For this computation, we regard P as a "fixed" polyomino in other words,
!    no reflections or rotations will be allowed.
!
!    An "embedding" of P into R is an offset (MI,NJ) such that 
!      P(I,J) = R(I+MI,J+NJ) 
!      for 1 <= I <= MP, 1 <= J <= NP, and 
!      for 0 <= MI <= MR-MP, 0 <= MJ <= NR-NP.
!    We can detect an embedding simply by taking what amounts to a kind of
!    dot product of P with a corresponding subregion of R.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 May 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) MR, NR, the number of rows and columns in
!    the representation of the region R.
!
!    Input, integer ( kind = 4 ) R[MR*NR], a matrix of 0's and 1's representing
!    the region.
!
!    Input, integer ( kind = 4 ) MP, NP, the number of rows and columns in the 
!    representation of the polyomino P.
!
!    Input, integer ( kind = 4 )v P[MP*NP], a matrix of 0's and 1's 
!    representing the polyomino.
!
!    Output, integer ( kind = 4 ) POLYOMINO_EMBED_NUMBER, the number of 
!    distinct embeddings of P into R.
!
  implicit none

  integer ( kind = 4 ) mp
  integer ( kind = 4 ) mr
  integer ( kind = 4 ) np
  integer ( kind = 4 ) nr

  integer ( kind = 4 ) mi
  integer ( kind = 4 ) nj
  integer ( kind = 4 ) number
  integer ( kind = 4 ) p(mp,np)
  integer ( kind = 4 ) pr
  integer ( kind = 4 ) r(mr,nr)
  integer ( kind = 4 ) srp

  number = 0
!
!  Count the 1's in P.
!
  pr = sum ( p(1:mp,1:np) )
!
!  For each possible (I,J) coordinate of the upper left corner of a subset of R,
!  see if it matches P.
!
  do mi = 0, mr - mp
    do nj = 0, nr - np
      srp = sum ( p(1:mp,1:np) * r(1+mi:mp+mi,1+nj:np+nj) )
      if ( srp == pr ) then
        number = number + 1
      end if   
    end do
  end do

  return
end
subroutine polyomino_enumerate_chiral ( n_data, order, number )

!*****************************************************************************80
!
!! POLYOMINO_ENUMERATE_CHIRAL counts chiral polyominoes (allowing holes).
!
!  Discussion:
!
!    Polyominoes are connected planar shapes formed by adjoining unit squares.
!
!    The number of unit squares in a polyomino is its order.
!
!    If we do not ignore reflections, but ignore rotations when comparing,
!    then we are considering the class of "fixed" polyominoes.  In that case,
!    for instance, there are 18 chiral polyominoes of order 5.
!
!    As the order increases, the number of polyominoes grows very rapidly.
!    The list offered here goes no further than order 28, but the later
!    numbers in the list are too large to represent as 32 byte integers. 
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2018
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Solomon Golomb,
!    Polyominoes: Puzzles, Patterns, Problems, and Packings,
!    Princeton University Press, 1996,
!    ISBN: 9780691024448
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0 
!    before the first call.  On each call, the routine increments N_DATA by 1, 
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ( kind = 4 ) ORDER, the order of a polyomino.
!
!    Output, integer ( kind = 8 ) NUMBER, the number of chiral polyominos 
!    of this order.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 31

  integer ( kind = 4 ) n_data
  integer ( kind = 8 ) number
  integer ( kind = 8 ), save, dimension ( n_max ) :: number_vec = (/ & 
    1_8, & 
    1_8, &
    1_8, &
    2_8, &
    7_8, &
    18_8, & 
    60_8, &
    196_8, &
    704_8, &
    2500_8, &
    9189_8, & 
    33896_8, &
    126759_8, &
    476270_8, &
    1802312_8, &
    6849777_8, & 
    26152418_8, &
    100203194_8, &
    385221143_8, &
    1485200848_8, &
    5741256764_8, & 
    22245940545_8, &
    86383382827_8, &
    336093325058_8, &
    1309998125640_8, &
    5114451441106_8, & 
    19998172734786_8, &
    78306011677182_8, &
    307022182222506_8, &
    1205243866707468_8, &
    4736694001644862_8 /)
  integer ( kind = 4 ) order
  integer ( kind = 4 ), save, dimension ( n_max ) :: order_vec = (/ & 
    0, &
    1,  2,  3,  4,  5, &
    6,  7,  8,  9, 10, &
   11, 12, 13, 14, 15, &
   16, 17, 18, 19, 20, &
   21, 22, 23, 24, 25, &
   26, 27, 28, 29, 30 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    order = 0
    number = 0_8
  else
    order = order_vec(n_data)
    number = number_vec(n_data)
  end if

  return
end
subroutine polyomino_enumerate_fixed ( n_data, order, number )

!*****************************************************************************80
!
!! POLYOMINO_ENUMERATE_FIXED counts fixed polyominoes (allowing holes).
!
!  Discussion:
!
!    Polyominoes are connected planar shapes formed by adjoining unit squares.
!
!    The number of unit squares in a polyomino is its order.
!
!    If we do not ignore reflections and rotations when comparing polyominoes,
!    then we are considering the class of "fixed" polyominoes.  In that case,
!    for instance, there are 65 fixed polyominoes of order 5.
!
!    As the order increases, the number of polyominoes grows very rapidly.
!    The list offered here goes no further than order 28, but the later
!    numbers in the list are too large to represent as 32 byte integers. 
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 April 2018
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Solomon Golomb,
!    Polyominoes: Puzzles, Patterns, Problems, and Packings,
!    Princeton University Press, 1996,
!    ISBN: 9780691024448
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0 
!    before the first call.  On each call, the routine increments N_DATA by 1, 
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ( kind = 4 ) ORDER, the order of a polyomino.
!
!    Output, integer ( kind = 8 ) NUMBER, the number of fixed polyominos 
!    of this order.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 29

  integer ( kind = 4 ) n_data
  integer ( kind = 8 ) number
  integer ( kind = 8 ), save, dimension ( n_max ) :: number_vec = (/ & 
    1_8, &
    1_8, &
    2_8, &
    6_8, &
    19_8, &
    63_8, &
    216_8, &
    760_8, &
    2725_8, &
    9910_8, &
    36446_8, &
    135268_8, &
    505861_8, &
    1903890_8, &
    7204874_8, &
    27394666_8, &
    104592937_8, &
    400795844_8, &
    1540820542_8, &
    5940738676_8, &
    22964779660_8, &
    88983512783_8, &
    345532572678_8, &
    1344372335524_8, &
    5239988770268_8, &
    20457802016011_8, &
    79992676367108_8, &
    313224032098244_8, &
    1228088671826973_8 /)
  integer ( kind = 4 ) order
  integer ( kind = 4 ), save, dimension ( n_max ) :: order_vec = (/ & 
    0, &
    1,  2,  3,  4,  5, &
    6,  7,  8,  9, 10, &
   11, 12, 13, 14, 15, &
   16, 17, 18, 19, 20, &
   21, 22, 23, 24, 25, &
   26, 27, 28 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    order = 0
    number = 0_8
  else
    order = order_vec(n_data)
    number = number_vec(n_data)
  end if

  return
end
subroutine polyomino_enumerate_free ( n_data, order, number )

!*****************************************************************************80
!
!! POLYOMINO_ENUMERATE_FREE counts free polyominoes (allowing holes).
!
!  Discussion:
!
!    Polyominoes are connected planar shapes formed by adjoining unit squares.
!
!    The number of unit squares in a polyomino is its order.
!
!    If we ignore reflections and rotations when comparing polyominoes,
!    then we are considering the class of "free" polyominoes.  In that case,
!    for instance, there are just 12 free polyominoes of order 5, the
!    so called "pentominoes".
!
!    As the order increases, the number of polyominoes grows very rapidly.
!    The list offered here goes no further than order 28, but the later
!    numbers in the list are too large to represent as 32 byte integers. 
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    11 April 2018
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Solomon Golomb,
!    Polyominoes: Puzzles, Patterns, Problems, and Packings,
!    Princeton University Press, 1996,
!    ISBN: 9780691024448
!
!  Parameters:
!
!    Input/output, integer ( kind = 4 ) N_DATA.  The user sets N_DATA to 0 
!    before the first call.  On each call, the routine increments N_DATA by 1, 
!    and returns the corresponding data; when there is no more data, the
!    output value of N_DATA will be 0 again.
!
!    Output, integer ORDER ( kind = 4 ), the order of a polyomino.
!
!    Output, integer NUMBER ( kind = 8 ), the number of free polyominos of 
!    this order.
!
  implicit none

  integer ( kind = 4 ), parameter :: n_max = 29

  integer ( kind = 4 ) n_data
  integer ( kind = 8 ) number
  integer ( kind = 8 ), save, dimension ( n_max ) :: number_vec = (/ & 
    1_8, &  
    1_8, &
    1_8, &
    2_8, &
    5_8, &
    12_8, &
    35_8, &
    108_8, &
    369_8, &
    1285_8, &
    4655_8, &
    17073_8, &
    63600_8, &
    238591_8, &
    901971_8, &
    3426576_8, &
    13079255_8, &
    50107909_8, &
    192622052_8, &
    742624232_8, &
    2870671950_8, &
    11123060678_8, &
    43191857688_8, &
    168047007728_8, &
    654999700403_8, &
    2557227044764_8, &
    9999088822075_8, &
    39153010938487_8, &
    153511100594603_8 /)
  integer ( kind = 4 ) order
  integer ( kind = 4 ), save, dimension ( n_max ) :: order_vec = (/ & 
    0, &
    1,  2,  3,  4,  5, &
    6,  7,  8,  9, 10, &
   11, 12, 13, 14, 15, &
   16, 17, 18, 19, 20, &
   21, 22, 23, 24, 25, &
   26, 27, 28 /)

  if ( n_data < 0 ) then
    n_data = 0
  end if

  n_data = n_data + 1

  if ( n_max < n_data ) then
    n_data = 0
    order = 0
    number = 0_8
  else
    order = order_vec(n_data)
    number = number_vec(n_data)
  end if

  return
end
subroutine polyomino_index ( m, n, p, pin )

!*****************************************************************************80
!
!! POLYOMINO_INDEX assigns an index to each nonzero entry of a polyomino.
!
!  Example:
!
!    P = 
!      1 0 1 1
!      1 1 1 0
!      0 1 1 0
!
!    PIN =
!      1 0 2 3
!      4 5 6 0
!      0 7 8 0
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 May 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns in the 
!    array that represents the polyomino.
!
!    Input, integer ( kind = 4 ) P(M*N), the polyomino.  It is assumed that 
!    every entry is a 0 or a 1.
!
!    Output, integer ( kind = 4 ) PIN(M*N), the index of each nonzero entry.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i;
  integer ( kind = 4 ) j;
  integer ( kind = 4 ) k;
  integer ( kind = 4 ) p(m*n)
  integer ( kind = 4 ) pin(m*n)

  k = 0
  do i = 1, m
    do j = 1, n
      if ( p(i+(j-1)*m) /= 0 ) then
        k = k + 1
        pin(i+(j-1)*m) = k
      else
        pin(i+(j-1)*m) = 0
      end if
    end do
  end do

  return
end
subroutine polyomino_lp_write ( filename, label, m, n, a, b )

!*****************************************************************************80
!
!! POLYOMINO_LP_WRITE writes an LP file for the polyomino problem.
!
!  Discussion:
!
!    The absurd and tedious process of trying to convince FORTRAN to print
!    data without extraneous blanks is a real comparative disadvantage.
!    I have to relearn it every time I need it.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) FILENAME, the output filename.
!
!    Input, character ( len = * ) LABEL, the problem title.
!
!    Input, integer ( kind = 4 ) M, the number of equations
!
!    Input, integer ( kind = 4 ) N, the number of variables.
!
!    Input, integer ( kind = 4 ) A(M,N), the coefficients.
!
!    Input, integer ( kind = 4 ) B(M), the right hand sides.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) b(m)
  character ( len = * ) filename
  logical first
  character ( len = * ) label
  integer ( kind = 4 ) output
  integer ( kind = 4 ) output_status
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  character ( len = 200 ) s
  character ( len = 80 ) s_j
  character ( len = 80 ) s_mag
!
!  Open the file.
!
  call get_unit ( output )

  open ( unit = output, file = filename, status = 'replace', &
    iostat = output_status )

  if ( output_status /= 0 ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'POLYOMINO_LP_WRITE - Error!'
    write ( *, '(a)' ) '  Could not open the output file.'
    stop 1
  end if

  write ( output, '(a)' ) label
  write ( output, '(a)' ) ''

  write ( output, '(a)' ) 'Maximize'
  write ( output, '(a)' ) '  Obj: 0'

  write ( output, '(a)' ) 'Subject to'

  do i = 1, m

    first = .true.

    do j = 1, n

      if ( a(i,j) /= 0 ) then

        s = ''

        if ( a(i,j) < 0 ) then
          s = trim ( s ) // ' -'
        else if ( .not. first ) then
          s = trim ( s ) // ' +'
        end if

        if ( abs ( a(i,j) ) /= 1 ) then
          write ( s_mag, '(i8)' ) abs ( a(i,j) )
          s_mag = adjustl ( s_mag )
          s = trim ( s ) // ' ' // trim ( s_mag )
        end if

        write ( s_j, '(i4)' ) j
        s_j = adjustl ( s_j )
        s = trim ( s ) // ' x' // s_j
        write ( output, '(a)', advance = 'no' ) trim ( s )

        first = .false.

      end if
    end do
    s = ' ='
    write ( s_mag, '(i8)' ) b(i)
    s_mag = adjustl ( s_mag )
    s = trim ( s ) // ' ' // trim ( s_mag )
    write ( output, '(a)', advance = 'no' ) trim ( s )
    write ( output, '(a)' ) ''
 
  end do

  write ( output, '(a)' ) 'Binary'
  s = ' '
  do j = 1, n
    write ( s_j, '(i4)' ) j
    s_j = adjustl ( s_j )
    s = trim ( s ) // ' x' // s_j
  end do
  write ( output, '(a)' ) trim ( s )

  write ( output, '(a)' ) 'End'
!
!  Close the file.
!
  close ( output )

  return
end
subroutine polyomino_monohedral_example_reid_size ( m, n )

!*****************************************************************************80
!
!! POLYOMINO_MONOHEDRAL_EXAMPLE_REID_SIZE returns the size of the system.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) M, N, the number of rows and columns
!    in the system.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  m = 9
  n = 10

  return
end
subroutine polyomino_monohedral_example_reid_system ( m, n, a, b )

!*****************************************************************************80
!
!! POLYOMINO_MONOHEDRAL_EXAMPLE_REID_SYSTEM sets up the Reid linear system.
!
!  Discussion:
!
!    This function sets up the linear system A*x=b associated with
!    the Reid polyomino tiling problem.
!
!    While it is desirable to have a general procedure that can automatically
!    deduce the linear system from the problem specification, for simplicity
!    in this example, we simply provide the linear system directly.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns in the system.
!
!    Output, integer ( kind = 4 ) A(M,N), the system matrix.
!
!    Output, integer ( kind = 4 ) B(M), the right hand side.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) a(m,n)
  integer ( kind = 4 ) b(m)
!
!  Note that the matrix is given in column major order.
!
  a = reshape ( (/ &
    1,1,0,0,0,0,0,0,2, &
    0,0,1,1,0,0,0,0,2, &
    0,0,0,1,1,0,0,0,2, &
    0,0,0,0,0,1,1,0,2, &
    0,0,0,0,0,0,1,1,2, &
    1,0,1,0,0,0,0,0,2, &
    0,1,0,1,0,0,0,0,2, &
    0,0,1,0,0,1,0,0,2, &
    0,0,0,1,0,0,1,0,2, &
    0,0,0,0,1,0,0,1,2 /), (/ m, n /) )

  b = (/ 1, 1, 1, 1, 1, 1, 1, 1, 8 /)

  return
end

subroutine polyomino_print ( m, n, p, label )

!*****************************************************************************80
!
!! POLYOMINO_PRINT prints a polyomino.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    29 April 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns in the 
!    representation of the polyomino P.
!
!    Input, integer ( kind = 4 ) P(M*N), a matrix of 0's and 1's representing 
!    the polyomino.  The matrix should be "tight", that is, there should be a
!    1 in row 1, and in column 1, and in row M, and in column N.
!
!    Input, character ( len = * ) LABEL, a title for the polyomino.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  character ( len = * ) label
  integer ( kind = 4 ) p(m,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) label
  write ( *, '(a)' ) ''

  if ( m < 1 .or. n < 1 ) then

    write ( *, '(a)' ) '  [ Null matrix ]'

  else

    do i = 1, m
      do j = 1, n
        write ( *, '(1x,i1)', advance = 'no' ) p(i,j)
      end do
      write ( *, '(a)' ) ''
    end do

  end if

  return
end
subroutine polyomino_transform ( m, n, p, rotate, reflect, mq, nq, q )

!*****************************************************************************80
!
!! POLYOMINO_TRANSFORM transforms a polyomino.
!
!  Discussion:
!
!    A polyomino can be rotated or reflected.
!
!    This program is given a polyomino and returns the resulting polyomino
!    after the specified reflection and rotation.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    17 April 2018
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the number of rows and columns in the 
!    representation of the polyomino P.
!
!    Input, integer ( kind = 4 ) P[M*N], a matrix of 0's and 1's representing 
!    the polyomino.  The matrix should be "tight", that is, there should be a
!    1 in row 1, and in column 1, and in row M, and in column N.
!
!    Input, integer ( kind = 4 ) ROTATE, is 0, 1, 2, or 3, the number of 90 
!    degree counterclockwise rotations to be applied.
!
!    Input, integer ( kind = 4 ) REFLECT, is 0 or 1.  If it is 1, then each row 
!    of the polyomino matrix is to be reflected before any rotations are 
!    performed.
!
!    Output, integer ( kind = 4 ) *MQ, *NQ, the number of rows and columns of 
!    the representation of the transformed polyomino
!
!    Output, integer ( kind = 4 ) Q[MQ*NQ], the representation of the 
!    transformed polyomino.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) k
  integer ( kind = 4 ) mq
  integer ( kind = 4 ) nq
  integer ( kind = 4 ) p(m,n)
  integer ( kind = 4 ) q(m*n)
  integer ( kind = 4 ) reflect
  integer ( kind = 4 ) rotate
  integer ( kind = 4 ) r
  integer ( kind = 4 ) s

  mq = m
  nq = n

  reflect = mod ( reflect, 2 )

  call i4mat_to_i4vec ( m, n, p, q )

  if ( reflect == 1 ) then
    call i4mat_flip_cols ( mq, nq, q )
  end if

  rotate = mod ( rotate, 4 )

  do k = 1, rotate
    call i4mat_transpose ( mq, nq, q )
    r = mq
    s = nq
    mq = s
    nq = r
    call i4mat_flip_rows ( mq, nq, q )
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
