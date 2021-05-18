subroutine cube_arbq ( degree, n, x, w )

!*****************************************************************************80
!
!! CUBE_ARBQ returns a quadrature rule for a cube.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DEGREE, the desired degree of exactness.
!    1 <= DEGREE <= 15.
!
!    Input, integer ( kind = 4 ) N, the number of points in the rule.
!    This value should be requested first from CUBE_ARBQ_SIZE.
!
!    Output, real ( kind = 8 ) X(3,N), the quadrature nodes.
!
!    Output, real ( kind = 8 ) W(N), the quadrature weights.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) d
  integer ( kind = 4 ) degree
  real ( kind = 8 ) volume
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(3,n)

  if ( degree == 1 ) then
    call rule01 ( n, x, w )
  else if ( degree == 2 ) then
    call rule02 ( n, x, w )
  else if ( degree == 3 ) then
    call rule03 ( n, x, w )
  else if ( degree == 4 ) then
    call rule04 ( n, x, w )
  else if ( degree == 5 ) then
    call rule05 ( n, x, w )
  else if ( degree == 6 ) then
    call rule06 ( n, x, w )
  else if ( degree == 7 ) then
    call rule07 ( n, x, w )
  else if ( degree == 8 ) then
    call rule08 ( n, x, w )
  else if ( degree == 9 ) then
    call rule09 ( n, x, w )
  else if ( degree == 10 ) then
    call rule10 ( n, x, w )
  else if ( degree == 11 ) then
    call rule11 ( n, x, w )
  else if ( degree == 12 ) then
    call rule12 ( n, x, w )
  else if ( degree == 13 ) then
    call rule13 ( n, x, w )
  else if ( degree == 14 ) then
    call rule14 ( n, x, w )
  else if ( degree == 15 ) then
    call rule15 ( n, x, w )
  else
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'CUBE_ARBQ - Fatal error!'
    write ( *, '(a)' ) '  Illegal value of DEGREE.'
    stop 1
  end if

  d = sum ( w(1:n) )
  volume = 8.0D+00
  w(1:n) = w(1:n) * volume / d

  return
end
subroutine cube_arbq_gnuplot ( n, x, header )

!*****************************************************************************80
!
!! CUBE_ARBQ_GNUPLOT: GNUPLOT plot of a quadrature rule in the symmetric cube.
!
!  Licensing:
!
!    This code is distributed under the GNU GPL license.
!
!  Modified:
!
!    11 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) N, the number of nodes.
!
!    Input, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Input, character * ( * ) HEADER, a string to be used to identify
!    the files created.
!
  implicit none

  integer ( kind = 4 ) n

  character * ( 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character * ( * ) header
  integer ( kind = 4 ) j
  character * ( 255 ) node_filename
  integer ( kind = 4 ) node_unit
  character * ( 255 ) plot_filename
  character * ( 255 ) vertex_filename
  integer ( kind = 4 ) vertex_unit
  real ( kind = 8 ) x(3,n)
!
!  Create the vertex file.
!
  call get_unit ( vertex_unit )
  vertex_filename = trim ( header ) // '_vertices.txt'
  open ( unit = vertex_unit, file = vertex_filename, &
    status = 'replace' )
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    -1.0D+00, -1.0D+00, -1.0D+00
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    +1.0D+00, -1.0D+00, -1.0D+00
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    +1.0D+00, +1.0D+00, -1.0D+00
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    -1.0D+00, +1.0D+00, -1.0D+00
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    -1.0D+00, -1.0D+00, -1.0D+00
  write ( vertex_unit, '(a)' ) ''
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    -1.0D+00, -1.0D+00, +1.0D+00
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    +1.0D+00, -1.0D+00, +1.0D+00
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    +1.0D+00, +1.0D+00, +1.0D+00
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    -1.0D+00, +1.0D+00, +1.0D+00
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    -1.0D+00, -1.0D+00, +1.0D+00
  write ( vertex_unit, '(a)' ) ''
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    -1.0D+00, -1.0D+00, -1.0D+00
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    -1.0D+00, -1.0D+00, +1.0D+00
  write ( vertex_unit, '(a)' ) ''
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    +1.0D+00, -1.0D+00, -1.0D+00
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    +1.0D+00, -1.0D+00, +1.0D+00
  write ( vertex_unit, '(a)' ) ''
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    +1.0D+00, +1.0D+00, -1.0D+00
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    +1.0D+00, +1.0D+00, +1.0D+00
  write ( vertex_unit, '(a)' ) ''
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    -1.0D+00, +1.0D+00, -1.0D+00
  write ( vertex_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) &
    -1.0D+00, +1.0D+00, +1.0D+00
  close ( unit = vertex_unit )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Created vertex file "' // &
    trim ( vertex_filename ) // '".'
!
!  Create node file.
!
  call get_unit ( node_unit )
  node_filename = trim ( header ) // '_nodes.txt'
  open ( unit = node_unit, file = node_filename, &
    status = 'replace' )
  do j = 1, n
    write ( node_unit, '(g14.6,2x,g14.6,2x,g14.6)' ) x(1:3,j)
  end do
  close ( unit = node_unit )
  write ( *, '(a)' ) '  Created node file "' // &
    trim ( node_filename ) // '".'
!
!  Create graphics command file.
!
  call get_unit ( command_unit )
  command_filename = trim ( header ) // '_commands.txt'
  open ( unit = command_unit, file = command_filename, &
    status = 'replace' )
  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // &
    trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  plot_filename = trim ( header ) // '.png'
  write ( command_unit, '(a)' ) 'set output "' // &
    trim ( plot_filename ) // '"'
  write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
  write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
  write ( command_unit, '(a)' ) 'set zlabel "<--- Z --->"'
  write ( command_unit, '(a)' ) &
    'set title "' // trim ( header ) // '"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set key off'
  write ( command_unit, '(a)' ) 'set view equal xyz'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'set timestamp'
  write ( command_unit, '(a)' ) 'splot "' // &
    trim ( vertex_filename ) // &
    '" with lines lw 3, \'
  write ( command_unit, '(a)' ) '     "' // &
    trim ( node_filename ) // '" with points pt 7 lt 0'
  close ( unit = command_unit )

  write ( *, '(a)' ) &
    '  Created command file "' // trim ( command_filename ) // '".'

  return
end
function cube_arbq_size ( degree )

!*****************************************************************************80
!
!! CUBE_ARBQ_SIZE returns the size of quadrature rule for a cube.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    07 July 2014
!
!  Author:
!
!    John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DEGREE, the desired degree of exactness.
!    1 <= DEGREE <= 15.
!
!    Output, integer ( kind = 4 ) CUBE_ARBQ_SIZE, the number of points in the
!    corresponding rule.
!
  implicit none

  integer ( kind = 4 ) cube_arbq_size
  integer ( kind = 4 ) degree
  integer ( kind = 4 ) n_save(15)

  save n_save

  data n_save / &
      1,   4,   6,  10,  13, &
     22,  26,  42,  50,  73, &
     84, 116, 130, 172, 190 /

  if ( degree < 1 .or. 15 < degree ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'CUBE_ARBQ_SIZE - Fatal error!'
    write ( *, '(a)' ) '  Illegal value of DEGREE.'
    stop 1
  end if

  cube_arbq_size = n_save ( degree )

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
subroutine lege3eva ( degree, z, pols )

!*****************************************************************************80
!
!! LEGE3EVA evaluates orthonormal polynomials in the cube.
!
!  Discussion:
!
!    The number of polynomials is
!  NPOLS = ( ( DEGREE + 1 ) * ( DEGREE + 2 ) * ( DEGREE + 3 ) ) / 6.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 July 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DEGREE, the maximum degree.
!
!    Input, real ( kind = 8 ) Z(3), the evaluation point.
!
!    Output, real ( kind = 8 ) POLS(NPOLS), the polynomial values.
!
  implicit none

  integer ( kind = 4 ) degree

  real ( kind = 8 ) f1(degree+1)
  real ( kind = 8 ) f2(degree+1)
  real ( kind = 8 ) f3(degree+1)
  integer ( kind = 4 ) kk
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  real ( kind = 8 ) pols(((degree+1)*(degree+2)*(degree+3))/6)
  real ( kind = 8 ) scale
  real ( kind = 8 ) t
  real ( kind = 8 ) z(3)

  call llegepols1 ( degree, z(1), f1 )
  call llegepols1 ( degree, z(2), f2 )
  call llegepols1 ( degree, z(3), f3 )

  kk = 0
  do m = 0, degree
    do n2 = 0, m
      do n1 = 0, n2
        kk = kk + 1
        pols(kk) = f1(m-n2+1) * f2(n2-n1+1) * f3(n1+1)
        scale = 1.0D+00
        t = 0.5D+00 * real ( 1 + 2 * n1, kind = 8 )
        scale = scale * sqrt ( t )
        t = 0.5D+00 * real ( 1 + 2 * n2 - 2 * n1, kind = 8 )
        scale = scale * sqrt ( t )
        t = 0.5D+00 * real ( 1 + 2 * m - 2 * n2, kind = 8 )
        scale = scale * sqrt ( t )
        pols(kk) = pols(kk) * scale
      end do
    end do
  end do

  return
end
subroutine llegepols1 ( degree, x, pols )

!*****************************************************************************80
!
!! LLEGEPOLS1 evaluates orthogonal polynomials on the symmetric interval.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    01 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) DEGREE, the maximum degree.
!
!    Input, real ( kind = 8 ) X, the evaluation point.
!
!    Output, real ( kind = 8 ) POLS(DEGREE+1)), the orthogonal
!    polynomials evaluated at X.
!
  implicit none

  integer ( kind = 4 ) degree

  integer ( kind = 4 ) k
  real ( kind = 8 ) pk
  real ( kind = 8 ) pkm1
  real ( kind = 8 ) pkp1
  real ( kind = 8 ) pols(degree+1)
  real ( kind = 8 ) x

  pkp1 = 1.0D+00
  pols(1) = pkp1

  if ( degree == 0 ) then
    return
  end if

  pk = pkp1
  pkp1 = x
  pols(2) = pkp1

  if ( degree == 1 ) then
    return
  end if

  do k = 1, degree - 1

    pkm1 = pk
    pk = pkp1
    pkp1 = ( real ( 2 * k + 1, kind = 8 ) * x * pk &
           - real (     k,     kind = 8 ) * pkm1 ) &
           / real (     k + 1, kind = 8 )

    pols(k+2) = pkp1

  end do

  return
end
subroutine r8mat_row_copy ( m, n, i, v, a )

!*****************************************************************************80
!
!! R8MAT_ROW_COPY copies a vector into a row of an R8MAT.
!
!  Discussion:
!
!    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    30 June 2014
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) M, N, the order of the matrix.
!
!    Input, integer ( kind = 4 ) I, the index of the row.
!    1 <= I <= M.
!
!    Input, real ( kind = 8 ) V(N), the row to be copied.
!
!    Input/output, real ( kind = 8 ) A(M,N), the matrix into which
!    the row is to be copied.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  real ( kind = 8 ) a(m,n)
  integer ( kind = 4 ) i
  real ( kind = 8 ) v(n)

  a(i,1:n) = v(1:n)

  return
end
subroutine rule01 ( n, x, w )

!*****************************************************************************80
!
!! RULE01 returns the rule of degree 1.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 1 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
       0.00000000000000000D+00 /
  data ys / &
       0.00000000000000000D+00 /
  data zs / &
       0.00000000000000000D+00 /
  data ws / &
       0.2828427124746189D+01 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  w(1:n) = ws(1:n)

  return
end
subroutine rule02 ( n, x, w )

!*****************************************************************************80
!
!! RULE02 returns the rule of degree 2.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 4 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
   -0.4380038262474289D+00, -0.4908226851347194D+00, &
    0.6313544088573617D+00,  0.8010097448936463D+00 /
  data ys / &
    0.6281382942978499D-01, -0.1242878410373149D+00, &
    0.8674258021608136D+00, -0.9533664350988082D+00 /
  data zs / &
   -0.8444012341886235D+00,  0.6401086714464984D+00, &
    0.1317904550701903D+00, -0.9165855436522309D-01 /
  data ws / &
    0.7590201299956376D+00, 0.9433497505402911D+00, &
    0.6278119491594441D+00, 0.4982452950508173D+00 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  w(1:n) = ws(1:n)

  return
end
subroutine rule03 ( n, x, w )

!*****************************************************************************80
!
!! RULE03 returns the rule of degree 3.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 6 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
    -0.5588399103385127D+00, 0.5588399103385127D+00, &
    -0.6245748884711381D+00, 0.6245748884711382D+00, &
    0.5311975164163759D+00, -0.5311975164163759D+00 /
  data ys / &
    -0.3347842945931215D+00, 0.3347842945931215D+00, &
    -0.3608970655525763D+00, 0.3608970655525763D+00, &
    -0.9062032945290301D+00, 0.9062032945290301D+00 /
  data zs / &
    -0.8055865032240838D+00, 0.8055865032240838D+00, &
    0.5832521303475051D+00, -0.5832521303475051D+00, &
    0.8103733422256782D-02, -0.8103733422256782D-02 /
  data ws / &
    0.4391890453578504D+00, 0.4391890453578504D+00, &
    0.5478113077968971D+00, 0.5478113077968971D+00, &
    0.4272132092183473D+00, 0.4272132092183473D+00 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  w(1:n) = ws(1:n)

  return
end
subroutine rule04 ( n, x, w )

!*****************************************************************************80
!
!! RULE04 returns the rule of degree 4.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 10 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
    0.5378961355313995D+00, 0.9230515460764596D+00, &
    -0.3017729559458644D+00, -0.4286245373611857D-01, &
    -0.4919281030834148D+00, -0.6331783925165083D+00, &
    0.3433859697316836D+00, 0.7159140285867756D+00, &
    -0.9634379969028102D+00, 0.9533570645781855D+00 /
  data ys / &
    -0.2921294864376587D+00, 0.1338187840340383D-01, &
    -0.6336712971880923D+00, 0.8637293692260690D+00, &
    0.6650083378678769D+00, -0.6579587923349518D+00, &
    0.1314461211661866D+00, -0.9334671020954506D+00, &
    0.2821274186555609D+00, 0.9271218349088852D+00 /
  data zs / &
    -0.9904747768071651D+00, 0.9034506944137641D+00, &
    0.7478682467863593D+00, 0.6980492706389707D+00, &
    -0.6792172628059848D+00, -0.5142053190660802D+00, &
    -0.1491834467493042D-01, -0.1124003569988050D-01, &
    0.4370157998712708D+00, -0.4433572115706369D+00 /
  data ws / &
    0.2015871863001034D+00, 0.1475782644463766D+00, &
    0.3427728700669111D+00, 0.2477511243780946D+00, &
    0.3268948471688580D+00, 0.3510375486490554D+00, &
    0.6548775481546729D+00, 0.2014557914699008D+00, &
    0.2371445377295266D+00, 0.1173274063826890D+00 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  w(1:n) = ws(1:n)

  return
end
subroutine rule05 ( n, x, w )

!*****************************************************************************80
!
!! RULE05 returns the rule of degree 5.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 13 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
    -0.2648598061927048D-17, 0.8182612016952856D+00, &
    -0.8182612016952856D+00, 0.3320846059655003D+00, &
    -0.3320846059655003D+00, -0.7192063268453662D+00, &
    0.7192063268453662D+00, 0.8748312857031093D+00, &
    -0.8748312857031093D+00, 0.6703438980154780D+00, &
    -0.6703438980154780D+00, -0.1288932781094893D-01, &
    0.1288932781094893D-01 /
  data ys / &
    0.2334819700920049D-17, 0.6996915596126003D+00, &
    -0.6996915596126003D+00, 0.8164106580812136D+00, &
    -0.8164106580812136D+00, 0.3823687870227997D+00, &
    -0.3823687870227997D+00, -0.4498159475180819D-01, &
    0.4498159475180820D-01, -0.8936284816367571D+00, &
    0.8936284816367571D+00, -0.6644842917167314D+00, &
    0.6644842917167314D+00 /
  data zs / &
    -0.2514317405785843D-17, -0.3279435833702815D+00, &
    0.3279435833702815D+00, 0.6999000775245028D+00, &
    -0.6999000775245028D+00, 0.7766614685968243D+00, &
    -0.7766614685968243D+00, 0.7066212170288233D+00, &
    -0.7066212170288233D+00, 0.1368716985635272D+00, &
    -0.1368716985635272D+00, 0.9082737241365967D+00, &
    -0.9082737241365967D+00 /
  data ws / &
    0.5954583420518295D+00, 0.1966394408155049D+00, &
    0.1966394408155048D+00, 0.1972244637798873D+00, &
    0.1972244637798874D+00, 0.2056648105428643D+00, &
    0.2056648105428642D+00, 0.1744527123656727D+00, &
    0.1744527123656728D+00, 0.1737372846337885D+00, &
    0.1737372846337885D+00, 0.1687656792094623D+00, &
    0.1687656792094623D+00 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  w(1:n) = ws(1:n)

  return
end
subroutine rule06 ( n, x, w )

!*****************************************************************************80
!
!! RULE06 returns the rule of degree 6.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 22 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
    -0.6520106181245787D+00, -0.8839112733197223D+00, &
    -0.4110873304651753D+00, -0.5483810510367449D+00, &
    0.2892216600948648D+00, 0.6342733723572165D+00, &
    0.9277349401977213D+00, 0.1959145493384946D+00, &
    0.9698859394138759D+00, 0.2417507986598153D+00, &
    0.8736828548839980D+00, -0.2473250700893141D+00, &
    -0.8359900749051526D+00, -0.7010811197269031D+00, &
    0.6850729043876912D+00, 0.8706999495161931D+00, &
    0.4433962826908943D+00, -0.2932096565623538D+00, &
    -0.2633910887960743D+00, -0.9524343530691857D+00, &
    0.5584942649156998D+00, -0.9550226927399118D+00 /
  data ys / &
    -0.9205233816190030D+00, 0.6254092057729300D+00, &
    -0.6525192767687262D+00, 0.6675277466837514D+00, &
    0.4977300744517685D+00, -0.8473122703810076D+00, &
    -0.2928745039351578D+00, -0.1497907581315417D+00, &
    0.3885854764755980D+00, -0.3158285918947235D+00, &
    0.8526532103620746D+00, 0.4591872919393147D+00, &
    -0.2440514778109439D+00, 0.4373390182884227D-01, &
    0.3426959807532622D+00, -0.5660924679198789D+00, &
    0.9081219217522495D+00, 0.9293150291789362D+00, &
    -0.7810388725033286D+00, 0.8260274928505457D+00, &
    -0.9767688598333689D+00, -0.8519217238424458D+00 /
  data zs / &
    -0.9890506976739800D+00, 0.9406229500121331D+00, &
    0.9356142377961858D+00, -0.9192764154799976D+00, &
    0.8878969382797651D+00, -0.7870867837034067D+00, &
    0.8660830979298149D+00, -0.8024709071632448D+00, &
    -0.8880711289349531D+00, 0.4529703911468836D+00, &
    0.5290490417189219D+00, -0.1854203758534000D+00, &
    -0.5963095626351139D+00, 0.4902173455917232D+00, &
    -0.2880115314573995D-01, -0.2057637646023516D+00, &
    -0.6042328075390052D+00, 0.4966484117970202D+00, &
    -0.2095900180453476D+00, -0.2268797125337812D+00, &
    0.6071604819572268D+00, 0.3850513071757548D+00 /
  data ws / &
    0.3178879561185747D-01, 0.4164076216957761D-01, &
    0.9948981399775451D-01, 0.9210852733177771D-01, &
    0.1401943913356885D+00, 0.7264966274445117D-01, &
    0.6525340756744888D-01, 0.2204173451372487D+00, &
    0.5094524937590311D-01, 0.2682685881175325D+00, &
    0.6962627523546265D-01, 0.2686201905068337D+00, &
    0.1619957777392285D+00, 0.2267482460842759D+00, &
    0.2223611442058410D+00, 0.1285943421530201D+00, &
    0.1171163684835690D+00, 0.1148693655898215D+00, &
    0.2337745730572889D+00, 0.6561672504107097D-01, &
    0.7668684384852936D-01, 0.5966072941200686D-01 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  w(1:n) = ws(1:n)

  return
end
subroutine rule07 ( n, x, w )

!*****************************************************************************80
!
!! RULE07 returns the rule of degree 7.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 26 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
    0.9181562358887896D+00, 0.4489608068872516D+00, &
    -0.3858744893115341D+00, -0.8375581084754601D+00, &
    0.8759137241702486D+00, 0.3582970649375837D+00, &
    -0.3606806413292835D-02, 0.8240785780348802D+00, &
    0.3960539405501686D+00, 0.6764493821739725D+00, &
    -0.7078657252089905D+00, -0.9910280819070909D+00, &
    -0.1644247691119722D-01, -0.9181562358887897D+00, &
    -0.4489608068872515D+00, 0.3858744893115342D+00, &
    0.8375581084754601D+00, -0.8759137241702486D+00, &
    -0.3582970649375837D+00, 0.3606806413292889D-02, &
    -0.8240785780348803D+00, -0.3960539405501686D+00, &
    -0.6764493821739724D+00, 0.7078657252089905D+00, &
    0.9910280819070909D+00, 0.1644247691119727D-01 /
  data ys / &
    -0.8730646210868583D+00, 0.3003747003093036D+00, &
    -0.8537024640925601D+00, -0.5290274183292351D+00, &
    -0.2186272204366992D+00, -0.8524776744046263D+00, &
    0.4528069580583293D+00, -0.7958792991210972D+00, &
    -0.3231996926348866D+00, 0.9204855453579330D+00, &
    0.1300957145548008D+00, -0.6507820069674347D+00, &
    0.9452977797065001D+00, 0.8730646210868583D+00, &
    -0.3003747003093037D+00, 0.8537024640925601D+00, &
    0.5290274183292349D+00, 0.2186272204366992D+00, &
    0.8524776744046263D+00, -0.4528069580583293D+00, &
    0.7958792991210972D+00, 0.3231996926348866D+00, &
    -0.9204855453579330D+00, -0.1300957145548009D+00, &
    0.6507820069674347D+00, -0.9452977797065000D+00 /
  data zs / &
    -0.8219732697110896D+00, 0.2702488394739114D+00, &
    -0.8405117117885644D+00, 0.8255893620180769D+00, &
    -0.2154761758370911D+00, -0.2372349012690425D+00, &
    -0.5304006751850494D+00, 0.5660607048365347D+00, &
    -0.8528418042921908D+00, -0.2586944089083810D+00, &
    -0.9317986614089844D+00, -0.5844907560422828D+00, &
    -0.9724859293584232D+00, 0.8219732697110896D+00, &
    -0.2702488394739114D+00, 0.8405117117885644D+00, &
    -0.8255893620180769D+00, 0.2154761758370910D+00, &
    0.2372349012690426D+00, 0.5304006751850494D+00, &
    -0.5660607048365347D+00, 0.8528418042921908D+00, &
    0.2586944089083810D+00, 0.9317986614089844D+00, &
    0.5844907560422828D+00, 0.9724859293584232D+00 /
  data ws / &
    0.2818669485625658D-01, 0.2457778545108605D+00, &
    0.7322107549897901D-01, 0.7474403008707498D-01, &
    0.1424052690706277D+00, 0.1464998318560304D+00, &
    0.2315603483757853D+00, 0.8404353770943794D-01, &
    0.1402956703321254D+00, 0.8519832938411190D-01, &
    0.7845038486431180D-01, 0.5021162675106791D-01, &
    0.3361890907642531D-01, 0.2818669485625655D-01, &
    0.2457778545108605D+00, 0.7322107549897898D-01, &
    0.7474403008707500D-01, 0.1424052690706277D+00, &
    0.1464998318560304D+00, 0.2315603483757854D+00, &
    0.8404353770943793D-01, 0.1402956703321254D+00, &
    0.8519832938411191D-01, 0.7845038486431183D-01, &
    0.5021162675106793D-01, 0.3361890907642534D-01 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  w(1:n) = ws(1:n)

  return
end
subroutine rule08 ( n, x, w )

!*****************************************************************************80
!
!! RULE08 returns the rule of degree 8.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 42 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
    -0.4475988003404910D+00, 0.1190967237337572D+00, &
    -0.4490161785347289D+00, 0.8651765155230445D+00, &
    0.6505429233922998D+00, 0.2589210616796688D+00, &
    0.9656989388220407D+00, 0.2260702905987988D+00, &
    -0.9447161047888400D+00, -0.3175860090502710D+00, &
    -0.5229105079658517D+00, 0.7843159489865733D+00, &
    -0.8185789055564340D+00, 0.2825863387132066D+00, &
    -0.7607858417694694D+00, 0.5866226772335372D+00, &
    0.9548626078195578D+00, -0.8132498468766962D+00, &
    -0.1585020553751056D+00, 0.4773375480094558D+00, &
    0.1284049655553692D+00, 0.1291886447672361D+00, &
    -0.5532190735732102D+00, -0.3228367990078276D+00, &
    0.6814247401185878D+00, 0.7557777797079770D+00, &
    -0.9000019573243127D+00, -0.2575843081671350D+00, &
    0.9631832063187018D+00, 0.6908303030190355D+00, &
    0.1821056025197366D+00, -0.8221919687508521D+00, &
    0.8588518473343788D+00, -0.8972339906902843D+00, &
    -0.5358934078144110D+00, -0.5529992683228151D+00, &
    -0.2001426466255732D-02, 0.9378537204512910D+00, &
    -0.9715016981164607D+00, -0.9829083600782837D+00, &
    0.9958977682871862D+00, 0.5952915553901705D+00 /
  data ys / &
    -0.4685908229573946D+00, -0.1796519363018103D+00, &
    0.8874428218089706D+00, 0.7669380410790798D+00, &
    -0.3322370805619765D+00, 0.4117713778755101D+00, &
    0.2677042158057942D+00, -0.6809206038797824D+00, &
    0.3428366182117646D+00, 0.9241061411826208D+00, &
    -0.9900623346092050D+00, -0.8016110436407171D+00, &
    0.4049960292973258D+00, 0.7556974668347299D+00, &
    -0.4707571060333716D+00, 0.1360876107297970D+00, &
    -0.9570347291094887D+00, -0.9999285474000510D+00, &
    -0.8417098248767442D+00, 0.7312551571808507D+00, &
    -0.8410385448275977D+00, 0.4262088590943245D+00, &
    -0.2958999490543837D+00, 0.3165179774473898D+00, &
    0.9500245471112998D+00, -0.6655395544878605D+00, &
    -0.5042046483103894D+00, -0.4401404838267679D-01, &
    0.1346519238430116D+00, -0.8848406668667183D-01, &
    -0.4368801480983787D+00, 0.8270836779598612D+00, &
    0.6421742345932883D+00, 0.1349979722216108D+00, &
    0.6936454950162161D+00, -0.8510571525270378D+00, &
    0.9631601523763739D+00, 0.9984857323994113D+00, &
    -0.8588323161863770D+00, 0.9585966990212117D+00, &
    -0.5890142881200693D+00, -0.9993743349557130D+00 /
  data zs / &
    -0.9772314603244738D+00, 0.9815070306506343D+00, &
    0.9720526068033335D+00, -0.9045817066028675D+00, &
    -0.9182437310533620D+00, -0.8841947772730918D+00, &
    0.9557023227491765D+00, 0.7227916414981562D+00, &
    0.8941205664955980D+00, -0.8999986131977584D+00, &
    0.9755454910061744D+00, 0.8657944704400204D+00, &
    -0.8703196425126886D+00, -0.5456656708443869D+00, &
    0.8251281474414323D+00, -0.3076953975629710D+00, &
    -0.8869272628530901D+00, -0.9230410589688908D+00, &
    0.5313897417255299D+00, 0.8095112746222346D+00, &
    -0.7431662230334861D+00, 0.1742031738076680D+00, &
    0.2874814736221014D+00, 0.6894604248684468D+00, &
    -0.4353446530984859D+00, -0.4153364490406374D+00, &
    -0.6226993610484064D+00, -0.5868262229626008D+00, &
    -0.6014685458288908D+00, 0.5571508078000768D+00, &
    -0.2048268508291324D-01, 0.4917939745803034D+00, &
    0.1369232794659417D+00, 0.7756109558580779D-02, &
    -0.2857541181158879D+00, -0.2330139351444999D+00, &
    0.2669563650277833D+00, 0.6741006875162401D+00, &
    0.4066025204718843D+00, -0.5565937248525152D+00, &
    0.2510714338774305D+00, 0.1349294493439425D+00 /
  data ws / &
    0.4086705877945525D-01, 0.5411955154283058D-01, &
    0.2383675183022706D-01, 0.2393935244990715D-01, &
    0.5813502964307203D-01, 0.7437498461386277D-01, &
    0.1984960102939174D-01, 0.6544081092072129D-01, &
    0.2659353157651851D-01, 0.3231832797003169D-01, &
    0.1304035568169426D-01, 0.3740726636480241D-01, &
    0.5743227080083509D-01, 0.7044771222211986D-01, &
    0.6713400141335041D-01, 0.1088370624350729D+00, &
    0.9430601398037914D-02, 0.9861052589481853D-02, &
    0.7173538891438543D-01, 0.8420277392308859D-01, &
    0.8298939034694322D-01, 0.1440733281281141D+00, &
    0.1295131217734937D+00, 0.1455803502566465D+00, &
    0.3601936507491939D-01, 0.8420800025743772D-01, &
    0.6488371524277974D-01, 0.1744028592454575D+00, &
    0.4595502978127373D-01, 0.1382478089544316D+00, &
    0.1725289949653584D+00, 0.6019513413223115D-01, &
    0.8271749698617868D-01, 0.9333260474256072D-01, &
    0.1346393673413504D+00, 0.9612888710317599D-01, &
    0.6100738206538034D-01, 0.1085509425473334D-01, &
    0.2871508442754113D-01, 0.1504859125563503D-01, &
    0.3740891876232848D-01, 0.4097311354933509D-01 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  w(1:n) = ws(1:n)

  return
end
subroutine rule09 ( n, x, w )

!*****************************************************************************80
!
!! RULE09 returns the rule of degree 9.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 50 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
    0.7677593774006314D+00, &
    -0.7677593774006314D+00, -0.3207821329562814D+00, &
    0.3207821329562814D+00, -0.4136209586513314D+00, &
    0.4136209586513314D+00, -0.6852061275740796D+00, &
    0.6852061275740795D+00, 0.9244405033583697D+00, &
    -0.9244405033583697D+00, -0.6411525460395265D+00, &
    0.6411525460395265D+00, -0.6524611766154725D+00, &
    0.6524611766154725D+00, -0.9637289125097334D+00, &
    0.9637289125097335D+00, -0.4042812911589966D-01, &
    0.4042812911589966D-01, 0.7010911265568170D+00, &
    -0.7010911265568170D+00, -0.4018375553483048D+00, &
    0.4018375553483048D+00, 0.2427999428831116D+00, &
    -0.2427999428831116D+00, -0.8888214064543165D+00, &
    0.8888214064543165D+00, 0.5686958127555934D+00, &
    -0.5686958127555934D+00, -0.1007305440999530D+00, &
    0.1007305440999530D+00, 0.9627142008988805D+00, &
    -0.9627142008988805D+00, 0.5575105029618763D+00, &
    -0.5575105029618763D+00, -0.2006401852932052D+00, &
    0.2006401852932052D+00, 0.1276245748755967D+00, &
    -0.1276245748755967D+00, 0.5324626645500558D+00, &
    -0.5324626645500558D+00, -0.8230657430079429D+00, &
    0.8230657430079429D+00, -0.9171428680981173D+00, &
    0.9171428680981173D+00, 0.9753289529764423D+00, &
    -0.9753289529764423D+00, 0.7278991004245323D+00, &
    -0.7278991004245322D+00, 0.9084671271535661D+00, &
    -0.9084671271535661D+00 /
  data ys / &
    -0.5215705733515856D-02, &
    0.5215705733515811D-02, -0.9255213178288733D+00, &
    0.9255213178288733D+00, 0.3254858593442050D+00, &
    -0.3254858593442050D+00, -0.8673453037549068D+00, &
    0.8673453037549068D+00, 0.1651688473834196D+00, &
    -0.1651688473834196D+00, 0.9161123256468909D+00, &
    -0.9161123256468909D+00, 0.8789047081433894D+00, &
    -0.8789047081433894D+00, 0.7092728961533591D+00, &
    -0.7092728961533591D+00, 0.5224015774226531D+00, &
    -0.5224015774226531D+00, 0.4986074979684522D+00, &
    -0.4986074979684522D+00, -0.5887106445666494D-01, &
    0.5887106445666495D-01, 0.4841822578088601D+00, &
    -0.4841822578088601D+00, -0.7052476161004777D+00, &
    0.7052476161004776D+00, -0.7991952932799359D-01, &
    0.7991952932799361D-01, 0.2884264730944422D+00, &
    -0.2884264730944421D+00, -0.5964266662509132D+00, &
    0.5964266662509132D+00, -0.7120073930331048D+00, &
    0.7120073930331048D+00, 0.8586009498349154D+00, &
    -0.8586009498349154D+00, 0.9369688457657286D+00, &
    -0.9369688457657286D+00, 0.6794094006908223D+00, &
    -0.6794094006908225D+00, 0.4202573751253162D+00, &
    -0.4202573751253164D+00, -0.1970879922320003D+00, &
    0.1970879922320003D+00, 0.8523907907745764D+00, &
    -0.8523907907745764D+00, 0.9938423815326598D+00, &
    -0.9938423815326598D+00, -0.9848158730090135D+00, &
    0.9848158730090135D+00 /
  data zs / &
    -0.9944420260442561D+00, &
    0.9944420260442561D+00, 0.9672106847608946D+00, &
    -0.9672106847608946D+00, -0.9450653904792801D+00, &
    0.9450653904792801D+00, -0.9208144764213119D+00, &
    0.9208144764213120D+00, 0.8857214694746194D+00, &
    -0.8857214694746194D+00, 0.8845112554423256D+00, &
    -0.8845112554423256D+00, -0.8251225389279271D+00, &
    0.8251225389279271D+00, -0.8150208317048079D+00, &
    0.8150208317048079D+00, 0.8824401782407778D+00, &
    -0.8824401782407778D+00, 0.5971438826916258D+00, &
    -0.5971438826916258D+00, -0.5274658210290475D+00, &
    0.5274658210290475D+00, -0.7239973446191893D+00, &
    0.7239973446191893D+00, 0.7834569443713458D+00, &
    -0.7834569443713458D+00, -0.6010889720637044D+00, &
    0.6010889720637044D+00, 0.5589543903569408D-01, &
    -0.5589543903569408D-01, -0.7052432618789399D+00, &
    0.7052432618789399D+00, -0.2951319795385468D+00, &
    0.2951319795385468D+00, -0.4212601390804162D+00, &
    0.4212601390804162D+00, 0.5022536274483731D+00, &
    -0.5022536274483730D+00, -0.3981899691586369D-01, &
    0.3981899691586369D-01, -0.3419107086504177D+00, &
    0.3419107086504178D+00, 0.1910346742820620D+00, &
    -0.1910346742820620D+00, 0.4032363262946108D+00, &
    -0.4032363262946109D+00, -0.3785656001274115D+00, &
    0.3785656001274115D+00, 0.8086398500198494D-02, &
    -0.8086398500198474D-02 /
  data ws / &
    0.2538811854621882D-01, &
    0.2538811854621883D-01, 0.1762240779978733D-01, &
    0.1762240779978734D-01, 0.4976713220360957D-01, &
    0.4976713220360960D-01, 0.2355615731022458D-01, &
    0.2355615731022460D-01, 0.2724595042792551D-01, &
    0.2724595042792553D-01, 0.2403649336578707D-01, &
    0.2403649336578707D-01, 0.2946300694577264D-01, &
    0.2946300694577266D-01, 0.1432705784656010D-01, &
    0.1432705784656010D-01, 0.7802654597787825D-01, &
    0.7802654597787825D-01, 0.6022424126025965D-01, &
    0.6022424126025969D-01, 0.1068682771411129D+00, &
    0.1068682771411130D+00, 0.9993782502170498D-01, &
    0.9993782502170505D-01, 0.3727854515440352D-01, &
    0.3727854515440351D-01, 0.9828312978563304D-01, &
    0.9828312978563307D-01, 0.1296245426718050D+00, &
    0.1296245426718050D+00, 0.3099443142302383D-01, &
    0.3099443142302385D-01, 0.9399083827155912D-01, &
    0.9399083827155913D-01, 0.8169878897651819D-01, &
    0.8169878897651825D-01, 0.5623588432426985D-01, &
    0.5623588432426987D-01, 0.1056612200118129D+00, &
    0.1056612200118130D+00, 0.9074609788069554D-01, &
    0.9074609788069565D-01, 0.6975340566577869D-01, &
    0.6975340566577869D-01, 0.2198686280707488D-01, &
    0.2198686280707488D-01, 0.2302735019253151D-01, &
    0.2302735019253151D-01, 0.1846925136114678D-01, &
    0.1846925136114678D-01 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  w(1:n) = ws(1:n)

  return
end
subroutine rule10 ( n, x, w )

!*****************************************************************************80
!
!! RULE10 returns the rule of degree 10.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 73 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
    -0.9290008391594218D+00, 0.4559324934613976D+00, &
    -0.8503323372224584D+00, -0.6258718000763578D+00, &
    -0.7657521094877171D+00, 0.5194124679448002D+00, &
    -0.1876523482951094D-01, 0.7781249769033519D+00, &
    0.7907391104275550D+00, 0.1931186493871086D-01, &
    0.7973052188120806D-01, 0.8940978313791650D+00, &
    -0.3969913823357744D+00, -0.5840731119485252D+00, &
    -0.3054403489443687D+00, -0.6250814570086429D+00, &
    -0.4273170491243774D+00, 0.8887351719726477D+00, &
    -0.1425902095867877D+00, 0.1007385849466660D+00, &
    0.9914273856893784D+00, -0.2110597230830005D+00, &
    -0.9344242484094948D+00, 0.9254796648599809D+00, &
    0.5871962886124195D+00, -0.5644486965116591D+00, &
    -0.3450006491762153D+00, -0.1925706173126046D+00, &
    0.6150095319350374D+00, -0.9410296227058319D+00, &
    0.3990765859738513D+00, 0.3675511374298872D+00, &
    0.9468259750893930D+00, -0.8323347102578033D+00, &
    0.9487564158646586D+00, 0.6918585765278729D-01, &
    0.7257633299128992D+00, -0.3310667096656268D+00, &
    0.3501561292254360D+00, -0.2984756141933171D+00, &
    0.7448197310529086D+00, 0.8904795041635960D-01, &
    -0.8524054154380974D+00, -0.9797468974234023D+00, &
    0.2069917638147098D+00, -0.9222673238089781D+00, &
    -0.7404783544230075D+00, 0.4462428682423911D+00, &
    0.2499725669820603D+00, -0.5978592395842010D+00, &
    0.3880147299726153D+00, 0.9540597896735835D+00, &
    -0.1204215373750881D+00, 0.5635716786856889D+00, &
    -0.6198158466921444D+00, 0.5423781905790268D+00, &
    0.2041052298129538D+00, 0.8615414935964518D+00, &
    0.9625611083095826D+00, -0.5668586728612423D+00, &
    -0.9565835991972781D+00, 0.9290333570079982D+00, &
    0.9603761783535766D+00, -0.9001174228174761D+00, &
    0.7409829822994354D+00, -0.9186328686279674D+00, &
    0.7988074968670636D+00, -0.8055374206467150D+00, &
    -0.2303785439930381D+00, -0.7165954822802608D+00, &
    0.7098003466268242D+00, -0.2370105920082338D+00, &
    -0.9973208321714092D+00 /
  data ys / &
    0.5674482687326375D+00, -0.2911223108663519D+00, &
    0.1975635105828826D+00, -0.6882273878784136D+00, &
    -0.9313977287710445D+00, -0.5339817326366849D+00, &
    0.5593088502110057D+00, 0.6938671668589624D+00, &
    0.7457324218656869D+00, -0.4591331058869646D-01, &
    0.5440689882793791D+00, -0.8616026786491190D+00, &
    0.7739289299329076D+00, -0.2114642239345504D+00, &
    -0.3131293885912573D-01, 0.9186402247572539D+00, &
    0.9877228633852757D+00, -0.8848784715526166D+00, &
    -0.8430339428373445D+00, -0.9121169131818918D+00, &
    0.8352108151249428D-01, -0.5108468353410851D+00, &
    0.7934672273226390D+00, 0.5843397847611630D-02, &
    0.1982224584099894D+00, 0.6529491925552273D+00, &
    0.3453783893258016D+00, -0.6965527925846071D+00, &
    -0.6030971553224019D+00, -0.9773971815452341D+00, &
    0.9306828192128160D+00, 0.1729189273247773D+00, &
    0.9423175295395478D+00, 0.3117471500716507D+00, &
    0.8484099142032112D+00, -0.4549656193618034D+00, &
    0.4364950089820075D+00, -0.2119737649763384D-01, &
    -0.7501696781614142D-01, 0.3377085041383336D+00, &
    0.5119386721405460D+00, -0.2861289818382340D+00, &
    -0.9435758438429453D+00, -0.5501358135716665D+00, &
    -0.8151761180652929D+00, -0.4382794463039796D+00, &
    0.1037779030310166D+00, -0.9327537847317822D+00, &
    0.7781924335471115D+00, -0.6696667136365475D+00, &
    0.9993628918113781D+00, 0.6532557117383904D+00, &
    0.8321683233238897D+00, 0.8340145881278882D+00, &
    -0.3990216364491641D+00, -0.7026952932032947D+00, &
    0.4592299742407868D+00, -0.4282015920212867D+00, &
    0.2004721513667550D+00, -0.8934779235340296D+00, &
    0.8942502209608590D+00, -0.8738897670233189D+00, &
    -0.5705526001256189D+00, -0.1379917314855049D+00, &
    -0.1665061725494578D+00, -0.7946830985787409D+00, &
    0.9767806321337382D+00, 0.9597550703525793D+00, &
    0.9603148869900205D+00, 0.6723297616898366D+00, &
    -0.9804400598708566D+00, -0.9949665182949334D+00, &
    0.4191106171156644D+00 /
  data zs / &
    -0.9933978864296309D+00, 0.9901550709677349D+00, &
    0.9670485250396404D+00, 0.9686386295527810D+00, &
    -0.9482984392355279D+00, -0.9781218402197481D+00, &
    -0.9668923135113735D+00, 0.9569807092335304D+00, &
    -0.9255203116327040D+00, -0.8549048193170219D+00, &
    0.9206982447618198D+00, 0.9211244686937629D+00, &
    -0.7317954338646777D+00, -0.9289922028324820D+00, &
    0.8753848332253249D+00, -0.8882119979065144D+00, &
    0.9878899913703325D+00, -0.8880356823987890D+00, &
    -0.8903407082095307D+00, 0.8876467178896359D+00, &
    -0.9463172167846347D+00, 0.7267804723276519D+00, &
    0.8338672039848504D+00, 0.8514657617058597D+00, &
    -0.8105375103476049D+00, 0.7553648186259019D+00, &
    -0.5850428048791508D+00, -0.1054105129348611D+00, &
    0.7438442367847414D+00, 0.8658386707444876D+00, &
    0.7640316811327564D+00, 0.6612763241373982D+00, &
    0.7191463486728814D+00, -0.7362541429366616D+00, &
    -0.6578813706843174D+00, -0.6173927832617907D+00, &
    0.5688448669683742D+00, -0.2424692431476031D+00, &
    -0.3220397059827600D+00, 0.2741662573733258D+00, &
    -0.4245710530722387D+00, 0.3779936020945108D+00, &
    -0.4890743188433689D+00, -0.8427007062812649D+00, &
    0.3110361053541443D+00, 0.7265683900043480D+00, &
    0.5114587028650807D+00, -0.6590556333897338D+00, &
    -0.5656407036165850D+00, -0.5801968526465481D+00, &
    -0.8814901582749934D+00, 0.3049921985976026D+00, &
    0.4604030935118299D+00, 0.1644483100006628D+00, &
    0.1392355684867057D+00, -0.1804588400155518D+00, &
    -0.1659257531038997D-01, -0.6408450861337968D+00, &
    -0.2283594679036603D+00, 0.5423020975283729D+00, &
    -0.5488666370681280D+00, -0.1935317969193018D+00, &
    0.4390831472700056D+00, -0.3197962461756297D+00, &
    0.1476513989767390D+00, 0.1104669057496399D+00, &
    -0.1823430458926981D+00, 0.3668414192631035D+00, &
    -0.2174815141367195D+00, -0.1332596398840131D+00, &
    0.4442289032147039D+00, -0.1887850946760386D+00, &
    0.2545601348113754D+00 /
  data ws / &
    0.7213361346970690D-02, 0.2333141303565080D-01, &
    0.1679006811176461D-01, 0.1914958033825363D-01, &
    0.7581033822063699D-02, 0.2434386571403711D-01, &
    0.2909344157431100D-01, 0.1700037166331166D-01, &
    0.1673233204518952D-01, 0.3947746719767090D-01, &
    0.3908321158531827D-01, 0.1015197562376277D-01, &
    0.2887452492906294D-01, 0.4051167157357858D-01, &
    0.5023577704859834D-01, 0.1585721420277247D-01, &
    0.7185787768043105D-02, 0.1081642505043930D-01, &
    0.3035331237698869D-01, 0.2526839495335092D-01, &
    0.8807297819671642D-02, 0.5593986671876056D-01, &
    0.1373773962571757D-01, 0.2661615086380097D-01, &
    0.5999969968705832D-01, 0.4972097477068091D-01, &
    0.6999983849508175D-01, 0.5598146788784706D-01, &
    0.5112821739971896D-01, 0.4853200937844245D-02, &
    0.2643720405354587D-01, 0.7488081622291798D-01, &
    0.8050832752047557D-02, 0.4291659968333356D-01, &
    0.1294625305397554D-01, 0.7403978691481330D-01, &
    0.5117832177948896D-01, 0.7869167027903835D-01, &
    0.7987922983101066D-01, 0.7935975973606037D-01, &
    0.5190634933748844D-01, 0.8887073925421256D-01, &
    0.1520654567592403D-01, 0.1437138357007375D-01, &
    0.5539120615704855D-01, 0.3308981979630965D-01, &
    0.6553371660845619D-01, 0.3169478052156868D-01, &
    0.6276849569730045D-01, 0.6211610103098184D-01, &
    0.1053947144115475D-01, 0.2331767126681452D-01, &
    0.6027945648014443D-01, 0.4978984285341077D-01, &
    0.7974451931076965D-01, 0.6688248399941439D-01, &
    0.9777913635381968D-01, 0.5098104982036934D-01, &
    0.2951246816860325D-01, 0.4353390845153333D-01, &
    0.1466253257039164D-01, 0.2081467883348036D-01, &
    0.2633254465708832D-01, 0.5423792444480278D-01, &
    0.8745492482441419D-01, 0.3119590040598747D-01, &
    0.1563786213616101D-01, 0.2105985290616827D-01, &
    0.3523362981701141D-01, 0.7539771307788777D-01, &
    0.1972567979376486D-01, 0.2513903851381411D-01, &
    0.2400953849626596D-01 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  w(1:n) = ws(1:n)

  return
end
subroutine rule11 ( n, x, w )

!*****************************************************************************80
!
!! RULE11 returns the rule of degree 11.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 84 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
    0.2499788650260321D+00, 0.3921769824433600D+00, &
    -0.7499317903601719D+00, 0.1970279946664021D+00, &
    -0.4574811189815491D+00, 0.7920994458748165D+00, &
    0.9496878040642897D+00, 0.5494991726029024D+00, &
    -0.9415698074379264D+00, -0.1549790418658323D+00, &
    0.9014007983667046D+00, -0.4493885995928424D+00, &
    -0.7095579485335214D+00, -0.8736487328036152D+00, &
    -0.3264862525811954D-01, -0.1103424065156252D+00, &
    -0.5637539669697259D+00, 0.2945721905451988D+00, &
    -0.8603062445393469D+00, -0.7507058748066501D+00, &
    -0.2780639680677569D+00, 0.7047555601238907D+00, &
    -0.6295944388807833D+00, -0.9095323899548270D+00, &
    0.2964917498074751D+00, 0.8851424078101294D+00, &
    -0.7219563370216030D+00, -0.9230518045006708D+00, &
    0.5631064783902135D+00, -0.9323802418890973D+00, &
    0.9285744095618478D+00, -0.3446313474069602D-01, &
    0.5285124795902665D+00, -0.6564153211279398D+00, &
    0.9670556758395562D+00, 0.9746827810441796D+00, &
    -0.1497145509400533D+00, 0.5126157047506474D-01, &
    0.5771162069337331D+00, -0.9882152664587444D+00, &
    0.8664710684798440D+00, 0.1554480104048762D+00, &
    -0.2499788650260319D+00, -0.3921769824433600D+00, &
    0.7499317903601719D+00, -0.1970279946664021D+00, &
    0.4574811189815492D+00, -0.7920994458748165D+00, &
    -0.9496878040642897D+00, -0.5494991726029023D+00, &
    0.9415698074379265D+00, 0.1549790418658323D+00, &
    -0.9014007983667046D+00, 0.4493885995928425D+00, &
    0.7095579485335214D+00, 0.8736487328036152D+00, &
    0.3264862525811960D-01, 0.1103424065156252D+00, &
    0.5637539669697259D+00, -0.2945721905451988D+00, &
    0.8603062445393470D+00, 0.7507058748066501D+00, &
    0.2780639680677569D+00, -0.7047555601238906D+00, &
    0.6295944388807833D+00, 0.9095323899548271D+00, &
    -0.2964917498074751D+00, -0.8851424078101294D+00, &
    0.7219563370216030D+00, 0.9230518045006707D+00, &
    -0.5631064783902135D+00, 0.9323802418890973D+00, &
    -0.9285744095618478D+00, 0.3446313474069607D-01, &
    -0.5285124795902665D+00, 0.6564153211279398D+00, &
    -0.9670556758395561D+00, -0.9746827810441796D+00, &
    0.1497145509400534D+00, -0.5126157047506470D-01, &
    -0.5771162069337331D+00, 0.9882152664587445D+00, &
    -0.8664710684798441D+00, -0.1554480104048762D+00 /
  data ys / &
    0.9380477277286925D+00, -0.3903208068416588D+00, &
    0.5539681565347837D+00, -0.9087002846713521D+00, &
    -0.6321385589135142D+00, 0.3182961009431599D+00, &
    -0.8236851175776945D+00, -0.8104327207737747D+00, &
    0.9326434060163864D+00, -0.4047646882920218D+00, &
    0.9551344910189569D+00, -0.8100975319542825D+00, &
    -0.2881465340470265D+00, 0.9393091372522897D+00, &
    0.7616291888714266D+00, -0.3792679791993436D+00, &
    -0.8198273601663221D+00, -0.6277875734879305D+00, &
    0.3601156105251353D+00, -0.6500193633731873D+00, &
    -0.1345149716413504D+00, 0.5453436457811436D+00, &
    0.9440747796649167D+00, -0.9592627915208141D+00, &
    -0.7303420755929871D-01, 0.8946123590730062D+00, &
    0.6073216328066294D-01, 0.2144997262319605D+00, &
    -0.1311566771466323D+00, -0.7186229429207273D+00, &
    -0.6749210564844365D+00, 0.9691840758487141D+00, &
    -0.6464321973320226D+00, 0.9661037973313142D+00, &
    0.5022924249551910D+00, 0.2011928667597957D+00, &
    -0.3711890466829925D+00, -0.7861623327708182D+00, &
    0.9831486697529307D+00, 0.1008010491222713D+00, &
    -0.6108831589158809D+00, 0.9546709186773804D+00, &
    -0.9380477277286926D+00, 0.3903208068416588D+00, &
    -0.5539681565347837D+00, 0.9087002846713521D+00, &
    0.6321385589135142D+00, -0.3182961009431599D+00, &
    0.8236851175776946D+00, 0.8104327207737747D+00, &
    -0.9326434060163864D+00, 0.4047646882920218D+00, &
    -0.9551344910189569D+00, 0.8100975319542825D+00, &
    0.2881465340470266D+00, -0.9393091372522897D+00, &
    -0.7616291888714266D+00, 0.3792679791993436D+00, &
    0.8198273601663221D+00, 0.6277875734879305D+00, &
    -0.3601156105251352D+00, 0.6500193633731873D+00, &
    0.1345149716413503D+00, -0.5453436457811436D+00, &
    -0.9440747796649167D+00, 0.9592627915208141D+00, &
    0.7303420755929881D-01, -0.8946123590730062D+00, &
    -0.6073216328066286D-01, -0.2144997262319605D+00, &
    0.1311566771466323D+00, 0.7186229429207273D+00, &
    0.6749210564844365D+00, -0.9691840758487141D+00, &
    0.6464321973320226D+00, -0.9661037973313142D+00, &
    -0.5022924249551909D+00, -0.2011928667597957D+00, &
    0.3711890466829925D+00, 0.7861623327708182D+00, &
    -0.9831486697529307D+00, -0.1008010491222714D+00, &
    0.6108831589158809D+00, -0.9546709186773804D+00 /
  data zs / &
    0.8462611120890736D+00, 0.3547611988896818D+00, &
    0.5262314749145942D-02, 0.7237196107659919D+00, &
    -0.6425814432975854D-01, 0.1803373124222549D+00, &
    -0.7073699162717987D+00, -0.3664557776165722D+00, &
    0.1016191298800923D+00, -0.4999025408268907D+00, &
    0.9145661354846110D+00, 0.6962396504425696D+00, &
    0.4859552045824018D+00, -0.9060323834426769D+00, &
    -0.1822470452558573D+00, 0.7016603041053766D+00, &
    -0.6424102177565745D+00, -0.7635438798952285D+00, &
    0.5612530704657699D+00, 0.9279296695895975D+00, &
    -0.8932000387343114D+00, 0.9433964716928596D+00, &
    -0.3855860228559991D+00, 0.8267083885708849D+00, &
    -0.2085191046423283D+00, 0.2867455231288707D+00, &
    -0.6303696136436402D+00, -0.9480808897684662D+00, &
    -0.8851760571455803D+00, 0.3677243413181205D+00, &
    0.5707827021209619D+00, 0.3308168458013439D+00, &
    0.8942058612668059D+00, 0.8491966568619232D+00, &
    0.6997888056053100D+00, -0.8582587623738921D+00, &
    0.9980904532339944D+00, -0.9929983508078530D+00, &
    -0.2607372382203097D+00, -0.2706521005603409D-01, &
    -0.9886712504226799D+00, -0.9981234658396176D+00, &
    -0.8462611120890736D+00, -0.3547611988896818D+00, &
    -0.5262314749145976D-02, -0.7237196107659919D+00, &
    0.6425814432975864D-01, -0.1803373124222550D+00, &
    0.7073699162717988D+00, 0.3664557776165722D+00, &
    -0.1016191298800922D+00, 0.4999025408268908D+00, &
    -0.9145661354846109D+00, -0.6962396504425696D+00, &
    -0.4859552045824018D+00, 0.9060323834426768D+00, &
    0.1822470452558573D+00, -0.7016603041053766D+00, &
    0.6424102177565745D+00, 0.7635438798952286D+00, &
    -0.5612530704657699D+00, -0.9279296695895975D+00, &
    0.8932000387343114D+00, -0.9433964716928596D+00, &
    0.3855860228559991D+00, -0.8267083885708848D+00, &
    0.2085191046423283D+00, -0.2867455231288708D+00, &
    0.6303696136436402D+00, 0.9480808897684662D+00, &
    0.8851760571455803D+00, -0.3677243413181204D+00, &
    -0.5707827021209619D+00, -0.3308168458013439D+00, &
    -0.8942058612668059D+00, -0.8491966568619232D+00, &
    -0.6997888056053101D+00, 0.8582587623738921D+00, &
    -0.9980904532339944D+00, 0.9929983508078530D+00, &
    0.2607372382203098D+00, 0.2706521005603400D-01, &
    0.9886712504226799D+00, 0.9981234658396176D+00 /
  data ws / &
    0.1322946412898770D-01, 0.7207978700091233D-01, &
    0.5018350497921869D-01, 0.2510540976305218D-01, &
    0.6381617344667434D-01, 0.5297826014608787D-01, &
    0.1107387819925981D-01, 0.4482499482382119D-01, &
    0.1156633883765873D-01, 0.8060095080318432D-01, &
    0.4983228901803450D-02, 0.3768915302119238D-01, &
    0.6105445139039273D-01, 0.6907527307462437D-02, &
    0.6642161794579643D-01, 0.6883290145149486D-01, &
    0.3765112561164340D-01, 0.5084567296013873D-01, &
    0.4165679702303787D-01, 0.1924373869332567D-01, &
    0.4590245342578355D-01, 0.2073261975977018D-01, &
    0.2506136162764897D-01, 0.6618443166679632D-02, &
    0.1133796667663198D+00, 0.2117476937280500D-01, &
    0.6105702586211274D-01, 0.1302086249789518D-01, &
    0.4287461768786696D-01, 0.2726768060699983D-01, &
    0.2693097474826901D-01, 0.2634041203981413D-01, &
    0.3577055224953832D-01, 0.1180436625397797D-01, &
    0.1895051399977877D-01, 0.1378259645552230D-01, &
    0.1414623939342679D-01, 0.1101406610125121D-01, &
    0.1973564062736466D-01, 0.2303337346288448D-01, &
    0.8809606694498163D-02, 0.6060743137742029D-02, &
    0.1322946412898769D-01, 0.7207978700091235D-01, &
    0.5018350497921865D-01, 0.2510540976305218D-01, &
    0.6381617344667437D-01, 0.5297826014608786D-01, &
    0.1107387819925980D-01, 0.4482499482382119D-01, &
    0.1156633883765873D-01, 0.8060095080318433D-01, &
    0.4983228901803448D-02, 0.3768915302119238D-01, &
    0.6105445139039275D-01, 0.6907527307462438D-02, &
    0.6642161794579643D-01, 0.6883290145149484D-01, &
    0.3765112561164340D-01, 0.5084567296013873D-01, &
    0.4165679702303786D-01, 0.1924373869332567D-01, &
    0.4590245342578356D-01, 0.2073261975977018D-01, &
    0.2506136162764896D-01, 0.6618443166679627D-02, &
    0.1133796667663198D+00, 0.2117476937280501D-01, &
    0.6105702586211271D-01, 0.1302086249789518D-01, &
    0.4287461768786696D-01, 0.2726768060699984D-01, &
    0.2693097474826901D-01, 0.2634041203981412D-01, &
    0.3577055224953834D-01, 0.1180436625397797D-01, &
    0.1895051399977878D-01, 0.1378259645552230D-01, &
    0.1414623939342678D-01, 0.1101406610125121D-01, &
    0.1973564062736465D-01, 0.2303337346288442D-01, &
    0.8809606694498165D-02, 0.6060743137742026D-02 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  w(1:n) = ws(1:n)

  return
end
subroutine rule12 ( n, x, w )

!*****************************************************************************80
!
!! RULE12 returns the rule of degree 12.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 116 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
    -0.7715535323463264D+00, 0.3118087350366243D-01, &
    0.7202332131024749D+00, 0.4477007231810433D+00, &
    0.4990977036666501D+00, 0.7276854447788592D+00, &
    0.5641552377783545D-01, -0.6804449077772355D+00, &
    -0.3147755705468945D-01, 0.9656886599696646D+00, &
    -0.5809498997793892D+00, -0.2289254671435773D+00, &
    0.7876771526454306D+00, -0.4068670890700770D+00, &
    -0.8972365649862598D+00, -0.3532895192280955D+00, &
    -0.9567571227498393D+00, -0.1216832508787129D+00, &
    0.5700762717211539D+00, 0.5051003810086915D+00, &
    0.9701289189066667D+00, -0.8467694257635688D+00, &
    -0.9792766700328013D+00, 0.9239050051895750D+00, &
    0.3182661293383706D+00, 0.2128643425667921D+00, &
    -0.9491244031998681D+00, 0.7992362723313403D+00, &
    -0.2460884124375043D+00, 0.1342871260315162D+00, &
    -0.2373280028547328D+00, -0.5865009282285342D+00, &
    0.4626440026822248D+00, 0.3297957744728636D+00, &
    -0.9420531528791128D+00, -0.4341804961950992D+00, &
    -0.9225452596590470D+00, 0.6871688089857060D+00, &
    0.2448760905505635D+00, 0.5741688284173365D+00, &
    -0.8594595285871465D+00, -0.5554936117139078D+00, &
    -0.6494964119774425D+00, 0.5732782989920666D+00, &
    0.3752071332901741D-02, 0.6435232718547543D+00, &
    0.8238997691962470D+00, 0.6248144351825363D-01, &
    -0.5415679713524282D+00, -0.6912095128076139D-01, &
    0.4168395206084785D+00, 0.7671340542346751D+00, &
    0.9499134274201164D+00, 0.6612862602874549D+00, &
    0.4988292488539028D+00, -0.3980774278492163D+00, &
    0.2143015046710522D+00, -0.8245870486521025D+00, &
    0.9547288142651947D+00, -0.8256353544128002D+00, &
    0.8043198685414620D+00, -0.6534154451891698D+00, &
    -0.9452902258113793D+00, -0.6217330929720845D+00, &
    -0.9665133496506871D+00, 0.1521701688630693D+00, &
    -0.9371839994014188D+00, 0.9495343847338622D+00, &
    -0.3463029817783037D+00, -0.7895101820283607D+00, &
    -0.8159632548392504D-01, 0.2855115686118832D+00, &
    0.9535458359476985D+00, -0.2998645589788054D+00, &
    0.8771573781780831D+00, 0.5455017995441601D+00, &
    0.9166715004534769D-01, 0.9320968927813329D+00, &
    -0.4371224793112961D+00, -0.8347365350568826D+00, &
    -0.6455342940659587D+00, 0.8555205523715417D+00, &
    -0.2072617599943643D+00, -0.2113416898671757D+00, &
    -0.9486945106212309D+00, -0.9718805025138264D+00, &
    -0.1940500299791751D-02, 0.8641147432354730D+00, &
    -0.7571701781562347D+00, 0.4947987121201377D+00, &
    0.2223337905054378D+00, 0.9496122896148295D+00, &
    0.8640108234726359D+00, 0.7680642862414261D+00, &
    0.4625223529706101D+00, 0.9726683521171305D+00, &
    -0.7964752179472936D+00, -0.7061109658827748D+00, &
    -0.8721608252584128D+00, -0.6991324850192889D+00, &
    0.9831710538535281D+00, 0.3063724124769707D+00, &
    0.9886078377543190D+00, -0.6436734499882067D+00, &
    -0.1696864839354078D+00, -0.9581984720461905D+00, &
    0.6404741270364127D+00, -0.9954098177933381D+00, &
    -0.9998048311370020D+00, -0.3608374768729438D+00, &
    -0.4193491067426698D+00, -0.2591511667594117D+00, &
    0.5950366411277217D+00, 0.8362890578433639D+00, &
    -0.9989746911020813D+00, 0.9819256903295290D+00 /
  data ys / &
    0.3846975186938493D+00, -0.6224498777026040D+00, &
    0.1248067846342114D+00, 0.8759502561317533D+00, &
    0.3319258914157911D+00, -0.5633971521527585D+00, &
    0.8534622243379885D+00, 0.9174064836961019D+00, &
    0.6613043682070445D-01, 0.8826351471721087D+00, &
    0.3382028556583220D+00, 0.7699804117571577D+00, &
    -0.3555309699183173D+00, 0.5163363528866298D+00, &
    0.9578029706408058D+00, 0.2101811715995720D+00, &
    -0.6596351003887070D+00, 0.2923658777723023D-01, &
    -0.2160515820663783D+00, -0.7325716420674306D+00, &
    -0.9320850402391798D+00, 0.2535810500632735D+00, &
    -0.3368010225765684D+00, 0.9401901116430753D+00, &
    0.7005289547119918D-01, -0.4318126034481278D+00, &
    -0.7540912429552113D+00, 0.7738338149718788D+00, &
    -0.2709207207715034D+00, -0.4558191555703927D+00, &
    -0.9010499919081281D+00, 0.5077988769546107D+00, &
    -0.3368014224099352D+00, -0.1248384730156639D+00, &
    -0.9290918735449377D+00, -0.7020388511680736D+00, &
    0.8388875287519405D+00, 0.3950015262650662D+00, &
    0.4698716842353510D+00, 0.7927540313537204D-01, &
    -0.4958172798073285D+00, 0.9158035761509341D-01, &
    0.6706864246998623D+00, 0.7918494529907293D+00, &
    -0.6319475420779336D+00, 0.6495695968654632D+00, &
    -0.9456860423718022D+00, -0.8989638999178421D+00, &
    -0.3735186503913843D+00, 0.4704635101750327D+00, &
    -0.8716224029261982D+00, 0.5966448338821769D+00, &
    0.2261581260452755D-01, -0.7330157165466431D+00, &
    -0.7471944764154522D+00, 0.9683805827306203D+00, &
    -0.5519164542917125D+00, -0.8971331427558276D+00, &
    -0.8173334319953630D+00, 0.8059812120592472D+00, &
    -0.6969456308437438D+00, -0.2340344455553026D+00, &
    -0.6410900230088896D+00, -0.7876018852914574D+00, &
    -0.2676406878547541D+00, 0.3656010440914417D+00, &
    0.7517669252844423D+00, 0.9299482185727768D+00, &
    0.8095208519693138D+00, -0.9105465384851248D-01, &
    0.5754559865383174D-01, 0.7987833771715905D+00, &
    0.4062222029583985D+00, -0.2917475248364298D+00, &
    0.8341598642457054D+00, 0.9565675836400581D+00, &
    0.9495501483452489D+00, -0.5027178324567889D+00, &
    -0.6094294091525906D+00, -0.2041586923215633D+00, &
    0.8735321107965007D+00, 0.8176411928945507D-01, &
    0.5798716521493317D+00, 0.9747533229160877D+00, &
    0.4095645350870887D+00, 0.2069198533007474D+00, &
    -0.8800808072488089D+00, -0.2586645109021137D+00, &
    -0.6284140921870087D+00, -0.9837391845052924D+00, &
    0.6897557755650331D+00, 0.6214627043620098D+00, &
    -0.9147932185865665D+00, 0.9683589014439020D+00, &
    0.9722720243469519D+00, 0.3545312716884971D+00, &
    -0.9265634816103003D+00, 0.9823681379288890D+00, &
    0.6027221797612410D+00, 0.1550378502770828D+00, &
    -0.7188423853605198D+00, -0.9880539732209580D+00, &
    -0.3538081678676123D+00, -0.9848881983673942D+00, &
    0.9503771401811550D+00, 0.9895536743716908D+00, &
    -0.9877214291859133D+00, 0.6282240608226360D-01, &
    0.8687973020715331D+00, -0.9993768714765972D+00, &
    -0.8816819132287129D+00, -0.9024706682879141D+00, &
    0.3779679396631332D+00, -0.1517734130495604D+00, &
    -0.9719850622545042D+00, -0.9258604067050853D+00 /
  data zs / &
    -0.9149704001000553D+00, -0.5996714764210891D+00, &
    0.7907026496157833D+00, 0.9860392703559183D+00, &
    -0.4660715186534219D-01, -0.1393752230506733D+00, &
    0.8674767286359658D+00, 0.5467250815629719D+00, &
    -0.9763780125306059D+00, 0.5121364231430013D+00, &
    -0.6798617661241456D+00, 0.6371639769399203D+00, &
    0.2198362375814198D+00, -0.9280616749626068D+00, &
    0.7168598119569591D+00, 0.7368659134196429D+00, &
    -0.6061233370696343D+00, -0.7956884470646045D+00, &
    0.6572737265408953D+00, -0.9403846902844000D+00, &
    -0.6276502147372000D+00, 0.1269919055587806D+00, &
    0.4130533850992282D+00, 0.9393352681302030D+00, &
    0.8990311398428759D+00, -0.9212501242203712D+00, &
    0.5134369934005911D+00, 0.6109131534473928D+00, &
    -0.4181668086036411D+00, 0.5711408979823358D+00, &
    0.1117558004736949D+00, 0.3763350218603200D+00, &
    -0.5509292330117498D+00, -0.1436673317026735D-01, &
    0.8947995961383289D+00, 0.5293478643731013D+00, &
    0.9681968180532952D+00, 0.3548727821114203D+00, &
    0.6370668273720668D+00, -0.7744664564128696D+00, &
    -0.1174975410572717D+00, -0.2455457407049606D+00, &
    0.8684746243106054D+00, -0.3023424151630013D+00, &
    -0.8384309459289455D-01, 0.9285129719767410D+00, &
    -0.1479858042465700D+00, -0.7406305201907566D+00, &
    0.2042255456501517D+00, 0.9724059801463540D+00, &
    -0.3537644656876312D+00, -0.6281373274195593D+00, &
    0.4150304386610159D+00, 0.8808795896002272D+00, &
    0.3686704712426682D+00, 0.9460942659633094D+00, &
    0.9681464254739912D+00, 0.1328695747867607D+00, &
    -0.9698622847731962D+00, -0.9424723737502007D+00, &
    -0.6724135642153274D+00, -0.9637049413487057D+00, &
    -0.9654826579439407D+00, -0.3959451854631504D+00, &
    0.9553150406508956D+00, -0.4818859928369797D+00, &
    0.2896997475970485D+00, -0.5366726979456555D+00, &
    -0.6510037274971031D+00, 0.6185934968970651D+00, &
    0.3210185330943278D+00, 0.2400288119511415D+00, &
    0.8510838518999654D+00, 0.8754236562515145D+00, &
    -0.9470610777853996D+00, -0.8205588369782968D+00, &
    -0.4018976232642799D+00, 0.7751784036421725D+00, &
    -0.8147980972514376D+00, -0.6601638637102712D+00, &
    -0.1260885622828708D+00, -0.3084734983651212D+00, &
    -0.8179658955267972D-01, 0.2700716241482205D+00, &
    0.7694412333217397D+00, -0.8836683466383671D+00, &
    0.7669737715528676D+00, -0.9137690852575695D+00, &
    0.8605953113236016D+00, 0.8953797328026272D+00, &
    -0.8573304152865556D+00, -0.1765576765676193D-01, &
    0.5663399409423517D+00, 0.8216340568717669D-01, &
    0.7178509062543948D+00, -0.8248755417862220D+00, &
    -0.8408410046283714D+00, -0.7367428078031174D+00, &
    -0.4590219429006754D+00, 0.9831224519425289D+00, &
    0.1099197404645035D+00, 0.2714915831470573D+00, &
    -0.4771292875453570D+00, 0.6410524347781664D+00, &
    -0.9808727711140862D+00, -0.1799508573122650D+00, &
    -0.8648319917262105D+00, -0.1821178559958520D+00, &
    -0.7478825522596463D+00, -0.4398682537286034D+00, &
    0.9929654026734756D+00, -0.9978596306712945D+00, &
    -0.9997568603576099D+00, 0.9997078506812050D+00, &
    -0.3559815310105911D+00, 0.9905772316230947D+00 /
  data ws / &
    0.8171123714265456D-02, 0.2263229388891795D-01, &
    0.1818677478454230D-01, 0.2980129445309951D-02, &
    0.4365304132620811D-01, 0.3243988845839241D-01, &
    0.1486444991990349D-01, 0.1409533365332470D-01, &
    0.1188012333452950D-01, 0.6000058026819710D-02, &
    0.3512238833735714D-01, 0.3098299265015942D-01, &
    0.3654756930376477D-01, 0.1847118507157365D-01, &
    0.5391012235288447D-02, 0.4074719736210774D-01, &
    0.1096224300316088D-01, 0.4081648743391735D-01, &
    0.4154456995823457D-01, 0.1320278412821872D-01, &
    0.4290449708656973D-02, 0.3535406067309509D-01, &
    0.1105110007450795D-01, 0.3228649704663156D-02, &
    0.2983835839394490D-01, 0.2451444151701184D-01, &
    0.1248137360474183D-01, 0.2177139751257475D-01, &
    0.6405596175169058D-01, 0.5434653731649390D-01, &
    0.3076101092170598D-01, 0.4899566852520795D-01, &
    0.5225345103961254D-01, 0.7166342203347331D-01, &
    0.3920984475787756D-02, 0.4137916341593958D-01, &
    0.3600172354407817D-02, 0.4755213384458860D-01, &
    0.5073623632857935D-01, 0.3944403305910345D-01, &
    0.3387054595129749D-01, 0.6277110525242416D-01, &
    0.2166338571221270D-01, 0.3747883688963586D-01, &
    0.6163661481210551D-01, 0.1666526164418470D-01, &
    0.1395113427475628D-01, 0.2347682793313850D-01, &
    0.6306339243839931D-01, 0.1556571594055037D-01, &
    0.3387962780929576D-01, 0.3268836056103089D-01, &
    0.2278931777487743D-01, 0.1962500582231501D-01, &
    0.4456347447201976D-01, 0.5576706603430151D-02, &
    0.1594423386163151D-01, 0.2035708135880945D-01, &
    0.3103484011599505D-02, 0.9044075785104641D-02, &
    0.2689758906199793D-01, 0.1586801972478047D-01, &
    0.5176595113431329D-02, 0.3858960164224838D-01, &
    0.5739931404252700D-02, 0.7158286709420723D-01, &
    0.1871157285391988D-01, 0.8194850948531980D-02, &
    0.3678122219124356D-01, 0.4250046085039932D-01, &
    0.8513849699040082D-01, 0.4993413782858739D-01, &
    0.1233822876422000D-01, 0.3941817987846824D-01, &
    0.7113418117354728D-02, 0.1194652514212529D-01, &
    0.2508023865178747D-01, 0.1725533956116014D-01, &
    0.3737010567296990D-01, 0.3668781342194600D-01, &
    0.3343163204554410D-01, 0.4496577144136969D-01, &
    0.7409539266110132D-01, 0.1793429116081015D-01, &
    0.1653960634734396D-01, 0.9190332852452304D-02, &
    0.2809796363000793D-01, 0.1777992760135489D-01, &
    0.2391632963314971D-01, 0.5749099213189723D-02, &
    0.3433774681484043D-01, 0.2274673898154641D-01, &
    0.1536942439600953D-01, 0.1430939257823392D-01, &
    0.1282919496053713D-01, 0.1121380440794668D-01, &
    0.1184676390611188D-01, 0.7982226989051386D-02, &
    0.3477385886958077D-01, 0.1190123605011125D-01, &
    0.1168229078012223D-01, 0.1343804099688911D-01, &
    0.1228891741931907D-01, 0.1036505178017157D-01, &
    0.5937174344287885D-02, 0.4133423282546477D-02, &
    0.6371010607497968D-02, 0.1313727761639429D-01, &
    0.3669992824757512D-02, 0.1078959526158836D-01, &
    0.7230671036845260D-02, 0.6406235552064114D-02, &
    0.1096476832530329D-01, 0.7917718162474640D-02, &
    0.3572933439799740D-02, 0.1541652661200926D-02 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  w(1:n) = ws(1:n)

  return
end
subroutine rule13 ( n, x, w )

!*****************************************************************************80
!
!! RULE13 returns the rule of degree 13.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 130 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
    0.4870864101401451D+00, -0.2374760548181477D-01, &
    -0.7693492847298501D+00, 0.9412905403090202D+00, &
    0.6911155930532483D+00, 0.7898594135857243D+00, &
    -0.7034504617756053D+00, 0.1116726714264779D+00, &
    -0.6392758447394441D+00, -0.8512618100723829D+00, &
    -0.6550935879174717D+00, -0.2012982100456206D-01, &
    0.2379647994307194D+00, -0.8601912819787483D+00, &
    -0.9232873932846022D+00, -0.1900861086818608D+00, &
    0.1929412791278925D-01, 0.7258444409271682D+00, &
    -0.8143255678954792D+00, 0.2946629110201063D+00, &
    -0.4322866997565211D+00, 0.9734181389553355D+00, &
    -0.8655112784848059D+00, -0.9486962755224994D+00, &
    -0.8639618309045872D+00, 0.4192783763456998D+00, &
    -0.6493595219283761D-01, -0.3132633581182750D+00, &
    -0.7107686198927078D+00, 0.1584847796276057D+00, &
    0.7962238411361664D+00, 0.7050836730161649D+00, &
    0.8581636882302615D+00, 0.9437903485136390D+00, &
    -0.9469415138106567D+00, 0.3693463627427371D+00, &
    -0.4815016166149078D+00, 0.6225323271333136D+00, &
    -0.1216882752396847D+00, 0.4861399922133740D+00, &
    -0.6588662398008378D+00, 0.8733371851679250D+00, &
    -0.5553986064438454D+00, 0.7785003762786959D+00, &
    -0.9809869293544266D+00, -0.9200430029249042D+00, &
    0.3304920778295448D+00, 0.3661210439903886D+00, &
    0.6228621851378309D+00, -0.6948006452003259D+00, &
    -0.9310681159823312D+00, 0.1927229482124844D+00, &
    -0.9395909910668155D+00, -0.8918846544837020D+00, &
    -0.5046559139572119D+00, -0.5501818316586664D+00, &
    -0.9816538375537931D+00, 0.5810740673825343D-01, &
    -0.9917476914762476D+00, 0.1619246931269223D+00, &
    0.4044274445105526D+00, 0.9788832392715772D+00, &
    0.2896262066021671D+00, -0.9979092625656895D+00, &
    -0.9953891308905386D+00, -0.4870864101401453D+00, &
    0.2374760548181472D-01, 0.7693492847298502D+00, &
    -0.9412905403090200D+00, -0.6911155930532483D+00, &
    -0.7898594135857244D+00, 0.7034504617756053D+00, &
    -0.1116726714264779D+00, 0.6392758447394442D+00, &
    0.8512618100723829D+00, 0.6550935879174717D+00, &
    0.2012982100456194D-01, -0.2379647994307196D+00, &
    0.8601912819787482D+00, 0.9232873932846023D+00, &
    0.1900861086818609D+00, -0.1929412791278927D-01, &
    -0.7258444409271683D+00, 0.8143255678954792D+00, &
    -0.2946629110201063D+00, 0.4322866997565212D+00, &
    -0.9734181389553355D+00, 0.8655112784848059D+00, &
    0.9486962755224994D+00, 0.8639618309045871D+00, &
    -0.4192783763456998D+00, 0.6493595219283750D-01, &
    0.3132633581182749D+00, 0.7107686198927078D+00, &
    -0.1584847796276057D+00, -0.7962238411361664D+00, &
    -0.7050836730161649D+00, -0.8581636882302615D+00, &
    -0.9437903485136390D+00, 0.9469415138106567D+00, &
    -0.3693463627427371D+00, 0.4815016166149078D+00, &
    -0.6225323271333137D+00, 0.1216882752396847D+00, &
    -0.4861399922133739D+00, 0.6588662398008377D+00, &
    -0.8733371851679250D+00, 0.5553986064438454D+00, &
    -0.7785003762786958D+00, 0.9809869293544267D+00, &
    0.9200430029249042D+00, -0.3304920778295449D+00, &
    -0.3661210439903887D+00, -0.6228621851378309D+00, &
    0.6948006452003260D+00, 0.9310681159823312D+00, &
    -0.1927229482124843D+00, 0.9395909910668155D+00, &
    0.8918846544837020D+00, 0.5046559139572118D+00, &
    0.5501818316586664D+00, 0.9816538375537931D+00, &
    -0.5810740673825336D-01, 0.9917476914762476D+00, &
    -0.1619246931269224D+00, -0.4044274445105527D+00, &
    -0.9788832392715772D+00, -0.2896262066021671D+00, &
    0.9979092625656895D+00, 0.9953891308905385D+00 /
  data ys / &
    -0.4125999616297640D+00, -0.3577512709695355D+00, &
    -0.4586571513966052D+00, -0.8809700870302296D+00, &
    0.6141953888878622D+00, -0.1098686336047354D-01, &
    -0.2992063293298189D+00, -0.5784543682045333D+00, &
    0.1546499308353232D+00, 0.4744749166521733D+00, &
    0.6850785787653841D+00, 0.3498227222335277D+00, &
    -0.3706543545438595D-01, 0.9489055210853593D+00, &
    -0.8258138282634399D+00, 0.8310925689243290D+00, &
    -0.2955546221774640D+00, -0.2274560569718798D+00, &
    -0.7326258705118668D+00, 0.6810827567696704D+00, &
    -0.1839671034672870D+00, 0.6471305653882479D+00, &
    0.4545843865988449D+00, 0.2460664244382031D-01, &
    0.1874945363707773D+00, -0.5593766578223933D+00, &
    -0.9791589336665545D+00, -0.6283651952819381D+00, &
    0.5755680670959202D+00, 0.2389500706336686D-01, &
    -0.8335047675899898D+00, 0.3839430357533925D+00, &
    -0.9053291152689182D+00, 0.3178523468094013D+00, &
    0.1336071580769665D-01, -0.2394172411300733D+00, &
    -0.9590729155294019D-01, 0.9385425560623889D+00, &
    0.7891616010496042D+00, -0.8504780607583017D+00, &
    -0.9475181655259435D+00, 0.3020909284966213D+00, &
    -0.9580268216765446D+00, -0.9705093056943978D+00, &
    0.7530029765856929D+00, -0.9658747597521009D+00, &
    0.8589417682173183D+00, 0.5576404948411705D+00, &
    -0.9193264723330097D+00, -0.7573749854479314D+00, &
    0.7034268760540773D+00, 0.9510178717882097D+00, &
    -0.9731097679191421D+00, -0.8322328408621713D+00, &
    0.6019176765989178D+00, -0.4092215952731691D+00, &
    -0.5199973715576629D+00, 0.8581532655611123D+00, &
    0.2298310606072314D+00, -0.8504134103774321D+00, &
    -0.9915729124045047D+00, 0.8271450907199669D+00, &
    -0.9986629495001677D+00, 0.5886511512141286D+00, &
    0.9903359117471948D+00, 0.4125999616297641D+00, &
    0.3577512709695354D+00, 0.4586571513966052D+00, &
    0.8809700870302297D+00, -0.6141953888878622D+00, &
    0.1098686336047354D-01, 0.2992063293298190D+00, &
    0.5784543682045332D+00, -0.1546499308353234D+00, &
    -0.4744749166521733D+00, -0.6850785787653841D+00, &
    -0.3498227222335276D+00, 0.3706543545438599D-01, &
    -0.9489055210853593D+00, 0.8258138282634397D+00, &
    -0.8310925689243290D+00, 0.2955546221774641D+00, &
    0.2274560569718798D+00, 0.7326258705118668D+00, &
    -0.6810827567696704D+00, 0.1839671034672870D+00, &
    -0.6471305653882478D+00, -0.4545843865988448D+00, &
    -0.2460664244382029D-01, -0.1874945363707773D+00, &
    0.5593766578223933D+00, 0.9791589336665545D+00, &
    0.6283651952819381D+00, -0.5755680670959203D+00, &
    -0.2389500706336671D-01, 0.8335047675899898D+00, &
    -0.3839430357533924D+00, 0.9053291152689182D+00, &
    -0.3178523468094013D+00, -0.1336071580769660D-01, &
    0.2394172411300734D+00, 0.9590729155294021D-01, &
    -0.9385425560623889D+00, -0.7891616010496042D+00, &
    0.8504780607583017D+00, 0.9475181655259436D+00, &
    -0.3020909284966213D+00, 0.9580268216765446D+00, &
    0.9705093056943976D+00, -0.7530029765856929D+00, &
    0.9658747597521009D+00, -0.8589417682173183D+00, &
    -0.5576404948411705D+00, 0.9193264723330097D+00, &
    0.7573749854479312D+00, -0.7034268760540774D+00, &
    -0.9510178717882097D+00, 0.9731097679191421D+00, &
    0.8322328408621713D+00, -0.6019176765989177D+00, &
    0.4092215952731690D+00, 0.5199973715576629D+00, &
    -0.8581532655611123D+00, -0.2298310606072313D+00, &
    0.8504134103774322D+00, 0.9915729124045047D+00, &
    -0.8271450907199669D+00, 0.9986629495001677D+00, &
    -0.5886511512141284D+00, -0.9903359117471947D+00 /
  data zs / &
    0.2510431589001390D+00, -0.4730112580495535D+00, &
    -0.8296554957987250D+00, 0.6042839304817023D+00, &
    -0.4354086139851164D+00, 0.7512711569117528D+00, &
    0.2580885268523619D+00, 0.7181855545442590D+00, &
    0.7258539919433724D+00, 0.5394422830180516D+00, &
    -0.3665937773187872D+00, -0.4287198875778774D+00, &
    -0.1391272946798063D+00, -0.2939636504110460D+00, &
    -0.8894944904099870D-01, 0.6509390582294805D+00, &
    -0.8295699704108563D+00, 0.9037134226828011D-03, &
    0.7543687304209290D+00, 0.7789156932196367D+00, &
    0.6834706616686410D+00, -0.2649602815663506D+00, &
    -0.6640360044523207D+00, 0.3841143167045120D+00, &
    0.9462427733191926D+00, -0.3779968053092916D+00, &
    0.7871565548827959D+00, 0.8953414085684869D+00, &
    -0.9510332692943319D+00, -0.9747087096347502D+00, &
    -0.6936472418684265D+00, -0.9725884697577191D+00, &
    -0.9605395058198848D+00, -0.7896659742434409D+00, &
    -0.9585765042296354D+00, 0.8915927965605104D+00, &
    -0.5323083877061187D+00, -0.9089350785066914D+00, &
    -0.8104031924041570D-01, 0.7186845896034352D+00, &
    0.7635825638284459D-01, 0.2475045724812137D+00, &
    -0.8458514443440915D+00, 0.9116524510899568D+00, &
    -0.9030128536354431D+00, 0.5786900078194604D+00, &
    -0.4876904077471467D+00, 0.9522425964634561D-01, &
    -0.2288092843749514D+00, -0.4933688094107099D+00, &
    0.6791308826513548D-01, 0.3643415241007317D+00, &
    -0.5743612468117230D+00, -0.9410337888158190D+00, &
    0.9256166802221193D+00, -0.9794608522159433D+00, &
    -0.7050722652352325D+00, 0.9838945816912786D+00, &
    -0.3456879759150185D+00, 0.9876394980104540D+00, &
    -0.8354581552777850D+00, -0.9782936349079702D+00, &
    0.3136198827810360D+00, 0.8542937538183871D+00, &
    0.4938372507517853D+00, -0.2510431589001390D+00, &
    0.4730112580495534D+00, 0.8296554957987250D+00, &
    -0.6042839304817023D+00, 0.4354086139851165D+00, &
    -0.7512711569117528D+00, -0.2580885268523619D+00, &
    -0.7181855545442589D+00, -0.7258539919433724D+00, &
    -0.5394422830180515D+00, 0.3665937773187872D+00, &
    0.4287198875778774D+00, 0.1391272946798063D+00, &
    0.2939636504110459D+00, 0.8894944904099869D-01, &
    -0.6509390582294805D+00, 0.8295699704108564D+00, &
    -0.9037134226827184D-03, -0.7543687304209290D+00, &
    -0.7789156932196368D+00, -0.6834706616686410D+00, &
    0.2649602815663507D+00, 0.6640360044523208D+00, &
    -0.3841143167045120D+00, -0.9462427733191926D+00, &
    0.3779968053092917D+00, -0.7871565548827959D+00, &
    -0.8953414085684869D+00, 0.9510332692943317D+00, &
    0.9747087096347502D+00, 0.6936472418684265D+00, &
    0.9725884697577191D+00, 0.9605395058198848D+00, &
    0.7896659742434410D+00, 0.9585765042296354D+00, &
    -0.8915927965605104D+00, 0.5323083877061187D+00, &
    0.9089350785066915D+00, 0.8104031924041578D-01, &
    -0.7186845896034351D+00, -0.7635825638284455D-01, &
    -0.2475045724812137D+00, 0.8458514443440915D+00, &
    -0.9116524510899567D+00, 0.9030128536354431D+00, &
    -0.5786900078194604D+00, 0.4876904077471467D+00, &
    -0.9522425964634557D-01, 0.2288092843749515D+00, &
    0.4933688094107098D+00, -0.6791308826513545D-01, &
    -0.3643415241007316D+00, 0.5743612468117228D+00, &
    0.9410337888158191D+00, -0.9256166802221192D+00, &
    0.9794608522159431D+00, 0.7050722652352326D+00, &
    -0.9838945816912786D+00, 0.3456879759150185D+00, &
    -0.9876394980104540D+00, 0.8354581552777850D+00, &
    0.9782936349079702D+00, -0.3136198827810360D+00, &
    -0.8542937538183870D+00, -0.4938372507517853D+00 /
  data ws / &
    0.3140524074284941D-01, 0.3981029681429966D-01, &
    0.1485446788498034D-01, 0.6244986380811999D-02, &
    0.2738837680317996D-01, 0.2157943912866181D-01, &
    0.3558093745023824D-01, 0.3212728314491558D-01, &
    0.3092422246858361D-01, 0.2343742079178917D-01, &
    0.3088702409027052D-01, 0.5359064880099586D-01, &
    0.6482379336742157D-01, 0.9118048913013883D-02, &
    0.1301789370977779D-01, 0.2655541823663328D-01, &
    0.3453297251160139D-01, 0.4411844784877157D-01, &
    0.1649537886932118D-01, 0.2909276637092253D-01, &
    0.4360788133113655D-01, 0.1053270755633742D-01, &
    0.2277866730141684D-01, 0.1989571475016660D-01, &
    0.1091026629930172D-01, 0.4918510213252013D-01, &
    0.8074020956961596D-02, 0.2335968213222367D-01, &
    0.1235756243300519D-01, 0.1499095494625247D-01, &
    0.1701688982544987D-01, 0.1042658914550453D-01, &
    0.4196082934549206D-02, 0.1415599321876098D-01, &
    0.6647555404704302D-02, 0.3129197633426045D-01, &
    0.5718706313414747D-01, 0.8214012330106898D-02, &
    0.4800378437465311D-01, 0.2511168810726739D-01, &
    0.1848081803186647D-01, 0.3560225217379276D-01, &
    0.9719778448629727D-02, 0.4568238333579292D-02, &
    0.3878678624072500D-02, 0.6252926394948910D-02, &
    0.3458846268652847D-01, 0.6444154518560745D-01, &
    0.2421665637314795D-01, 0.3403670963655146D-01, &
    0.2101561337278881D-01, 0.2321281908800168D-01, &
    0.4914553718764686D-02, 0.6665400363164401D-02, &
    0.2231229502988629D-01, 0.1251715201913546D-01, &
    0.9481650136297978D-02, 0.8059627544061664D-02, &
    0.1180468380164612D-01, 0.7820767064272208D-02, &
    0.6958745625819240D-02, 0.2234878561766285D-02, &
    0.1024235452666042D-01, 0.5000286844891411D-02, &
    0.2657409809447695D-02, 0.3140524074284939D-01, &
    0.3981029681429966D-01, 0.1485446788498035D-01, &
    0.6244986380811991D-02, 0.2738837680317996D-01, &
    0.2157943912866178D-01, 0.3558093745023824D-01, &
    0.3212728314491559D-01, 0.3092422246858360D-01, &
    0.2343742079178915D-01, 0.3088702409027051D-01, &
    0.5359064880099586D-01, 0.6482379336742154D-01, &
    0.9118048913013897D-02, 0.1301789370977778D-01, &
    0.2655541823663329D-01, 0.3453297251160137D-01, &
    0.4411844784877157D-01, 0.1649537886932117D-01, &
    0.2909276637092255D-01, 0.4360788133113656D-01, &
    0.1053270755633743D-01, 0.2277866730141684D-01, &
    0.1989571475016661D-01, 0.1091026629930171D-01, &
    0.4918510213252012D-01, 0.8074020956961608D-02, &
    0.2335968213222367D-01, 0.1235756243300520D-01, &
    0.1499095494625249D-01, 0.1701688982544985D-01, &
    0.1042658914550453D-01, 0.4196082934549209D-02, &
    0.1415599321876098D-01, 0.6647555404704300D-02, &
    0.3129197633426043D-01, 0.5718706313414745D-01, &
    0.8214012330106886D-02, 0.4800378437465309D-01, &
    0.2511168810726739D-01, 0.1848081803186646D-01, &
    0.3560225217379277D-01, 0.9719778448629732D-02, &
    0.4568238333579302D-02, 0.3878678624072491D-02, &
    0.6252926394948914D-02, 0.3458846268652847D-01, &
    0.6444154518560744D-01, 0.2421665637314794D-01, &
    0.3403670963655148D-01, 0.2101561337278882D-01, &
    0.2321281908800168D-01, 0.4914553718764693D-02, &
    0.6665400363164389D-02, 0.2231229502988629D-01, &
    0.1251715201913550D-01, 0.9481650136297967D-02, &
    0.8059627544061670D-02, 0.1180468380164611D-01, &
    0.7820767064272205D-02, 0.6958745625819234D-02, &
    0.2234878561766282D-02, 0.1024235452666043D-01, &
    0.5000286844891406D-02, 0.2657409809447699D-02 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  w(1:n) = ws(1:n)

  return
end
subroutine rule14 ( n, x, w )

!*****************************************************************************80
!
!! RULE14 returns the rule of degree 14.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 172 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
    0.3724711910241946D-01, 0.4142659262186393D+00, &
    0.5177982339713144D+00, 0.3604423376125923D+00, &
    0.8256117236986897D+00, 0.4265362215919388D+00, &
    -0.5205630040142383D+00, 0.5476096231885307D+00, &
    -0.3908413316164978D-01, -0.6869205136187482D+00, &
    -0.8269790632507413D+00, -0.3077403136300738D+00, &
    -0.4871197763974943D+00, -0.6602942921439789D+00, &
    -0.1996644422759564D+00, -0.2612979535504019D+00, &
    0.5138865516799961D+00, -0.8650532980097219D+00, &
    0.5098273324253541D+00, 0.7674795450461877D+00, &
    0.1907820276114263D+00, -0.7413413727558544D+00, &
    0.8578428692155201D+00, -0.9483245969337234D+00, &
    0.6622911479023382D+00, 0.1082533914200937D+00, &
    0.9727081876507619D+00, 0.5564069612990988D+00, &
    0.6174233759718846D+00, -0.7840704914833193D+00, &
    -0.3863554802732841D+00, -0.7642023424993235D+00, &
    -0.7532542829872437D+00, -0.8018433723045076D+00, &
    0.3770659555409062D+00, -0.9491540989399579D+00, &
    -0.9382558523915473D+00, -0.9808828605653619D+00, &
    -0.4844689691834917D+00, 0.9256039531559204D+00, &
    -0.8709952382834715D-01, -0.5396392548668583D+00, &
    0.1368025517956275D+00, 0.3374041492353974D+00, &
    0.7068715546921336D+00, -0.4085110348118505D+00, &
    -0.9681798506556836D+00, -0.1549674800732925D+00, &
    0.9150816905013831D+00, 0.5375252305267589D-01, &
    -0.5579823240421485D+00, 0.8969270740962718D-02, &
    0.7014120009018806D+00, 0.1912491108449427D+00, &
    0.5828147015903538D+00, 0.9521221772289220D+00, &
    -0.1994976199453465D+00, -0.8137190118036577D+00, &
    -0.5666026693931812D+00, -0.7926656084386846D+00, &
    -0.6502780637374249D+00, 0.1603598833824042D+00, &
    -0.9646753205843002D+00, 0.1874185821317708D+00, &
    0.8078893967819246D+00, 0.9821198889247245D+00, &
    -0.9134820612323216D+00, 0.9315563412192052D+00, &
    0.9673991240002898D-02, -0.5917684536649436D+00, &
    -0.5910952945368380D+00, -0.9004251112937950D+00, &
    0.4666785637176430D+00, -0.9279892538634408D+00, &
    0.5165215046136072D+00, -0.6522976391673276D+00, &
    -0.4635151732777533D+00, -0.2216103796843620D+00, &
    0.9720841352166502D+00, 0.8697669064712771D+00, &
    -0.7844145244283243D+00, 0.5653272793694807D+00, &
    0.5670262199937089D+00, -0.8760558996804383D+00, &
    -0.6568661047943578D+00, -0.6431016384228920D-01, &
    0.2699910056033774D+00, -0.3193920506608590D+00, &
    0.5594292856531002D+00, -0.8625783835676980D+00, &
    0.9642093306328916D+00, 0.9564210050320944D+00, &
    -0.5256008382082430D+00, 0.6831869769415302D+00, &
    -0.5564497515417263D+00, -0.8665833611119687D+00, &
    0.2119314545737717D+00, 0.6499134147160973D+00, &
    0.8416070492408827D+00, -0.8860664019820089D+00, &
    0.8630542601243912D+00, 0.4006206217350135D+00, &
    -0.3255422740709495D+00, 0.3889737632509294D+00, &
    -0.4031111086863209D+00, -0.1026555049543291D+00, &
    -0.2154587129488632D+00, -0.9587635011613380D+00, &
    -0.6781849005122116D+00, 0.6431694453965539D-02, &
    0.5990879315696672D+00, 0.2604246381903720D+00, &
    -0.2205299895937927D+00, 0.7360523872792837D+00, &
    0.9141664583516047D+00, 0.6460945155487459D+00, &
    0.8261067491745480D+00, 0.2582806673903185D+00, &
    0.7992011614665788D+00, 0.1952759431439480D+00, &
    -0.1404821582745676D+00, -0.9719800694682158D+00, &
    -0.9082932030606901D+00, -0.8069621339496692D+00, &
    0.5138739584252263D+00, -0.4508670463586423D+00, &
    0.7460163089423278D+00, 0.8559732256227004D+00, &
    0.2619517671437559D+00, -0.7178069404428022D+00, &
    0.7768409833572711D+00, 0.9566352889085462D+00, &
    -0.9129931899686473D+00, -0.2027074937002416D+00, &
    -0.9767056206307869D+00, -0.1953671488795780D+00, &
    0.9356675487713187D+00, 0.2733263440546579D-01, &
    0.8541785574421273D+00, -0.8509341178434021D+00, &
    -0.8389589448490465D+00, -0.9114666802875271D+00, &
    -0.2369922070544546D+00, 0.8352761372704256D+00, &
    0.6990375376349731D+00, -0.9825936001862040D+00, &
    0.9890254163849905D+00, 0.4299873858805139D+00, &
    0.9084398364888665D+00, -0.4942757972549205D+00, &
    0.1276694562023527D+00, 0.8433252017493162D+00, &
    0.9812363871492615D+00, -0.5112868612432091D+00, &
    -0.9919630673934499D+00, 0.9888971621532768D+00, &
    0.9906786057375682D+00, 0.9291016994895615D+00, &
    -0.9930351279012178D+00, -0.4368492693215694D+00, &
    -0.9926580930992508D+00, 0.9810341338091584D+00, &
    0.2829487311079641D+00, 0.9951522144360470D+00, &
    -0.9467788426833306D+00, 0.6231428267482866D+00, &
    -0.9966694864427889D+00, -0.5545516866973424D+00, &
    -0.2200546924714422D+00, 0.9855361391127346D+00, &
    0.1242997310669405D+00, -0.2411466505640982D-01 /
  data ys / &
    0.4603796517412539D+00, 0.7850928815883216D+00, &
    0.5139455507370757D+00, 0.9223890225451771D+00, &
    0.8434522866134452D+00, 0.6558862622340167D-01, &
    -0.8559434795169205D+00, -0.8987656715485196D+00, &
    0.4503144430486827D+00, -0.2618729087942862D+00, &
    0.9452894304667214D+00, -0.7280261316218042D+00, &
    -0.2338055905041149D+00, -0.4867015686038832D+00, &
    -0.8999180244059493D+00, 0.8333786735111544D-01, &
    -0.8337854025340903D+00, -0.3449380786557670D+00, &
    -0.3067078127421212D+00, -0.3509360716139491D+00, &
    -0.8655219480345354D+00, 0.1796817314118926D-01, &
    -0.6893079251914401D+00, -0.1756439435663739D+00, &
    -0.1729573648651443D+00, -0.8312838577052795D+00, &
    0.8465219729055495D+00, -0.7218336696759129D+00, &
    -0.8968669124158696D-01, 0.1072951525625464D+00, &
    0.2674684823317927D+00, 0.4494767718386262D+00, &
    -0.8467786081052485D+00, 0.4339653736633297D+00, &
    0.3042380260051257D+00, 0.5590699107221699D+00, &
    -0.5068967389653932D+00, -0.2405947127531271D+00, &
    0.8437874725090614D+00, -0.9017396521006380D+00, &
    0.8482922518824456D+00, -0.6591372127582090D+00, &
    0.1703921286880628D+00, 0.9563525832577533D+00, &
    0.9739918780282115D+00, -0.7115110177234589D+00, &
    -0.9751728996744083D+00, -0.9631468160421225D+00, &
    0.6009999468106125D+00, -0.5288962426219667D+00, &
    0.4595578349581757D+00, 0.9432670812899058D+00, &
    0.7399828097883627D+00, -0.4141604710166898D+00, &
    -0.4084225192010409D+00, -0.3386216837958897D+00, &
    -0.6001665103475902D+00, -0.8022895195480112D+00, &
    -0.3398023662469815D+00, 0.8186709874482936D+00, &
    -0.9024700874428341D+00, -0.8294025892178960D+00, &
    0.3513024770242288D+00, 0.8650597368535108D+00, &
    -0.8463680274683520D+00, -0.7300261016180669D+00, &
    -0.9758837733429508D+00, 0.9751575986713164D+00, &
    0.7126310344823681D+00, 0.9451735803803867D+00, &
    0.1370096023427370D+00, -0.9445191743918876D+00, &
    -0.5896755668296407D+00, -0.1197749258942398D+00, &
    0.5242071257866383D+00, 0.9051990022441897D+00, &
    0.1957941852390964D+00, 0.5375124860468565D+00, &
    0.8696281579006412D+00, 0.9780419188336278D+00, &
    -0.4600808747557152D+00, -0.6775300402478708D+00, &
    0.2563064109512353D+00, -0.6053293404401969D+00, &
    0.7731104690643289D+00, -0.9394448657791105D+00, &
    -0.1604465004198996D+00, 0.9502436474270409D+00, &
    0.8261506546206309D+00, -0.8150926456217005D+00, &
    0.9776649598956791D-01, 0.8412957281351358D+00, &
    0.7214572271897897D+00, -0.8985831295061654D+00, &
    -0.9335211793245841D+00, 0.7067653633526974D-02, &
    -0.3615585073957835D+00, 0.2972904877318600D+00, &
    -0.4095443751975350D+00, 0.9558903952389940D+00, &
    0.8951478996227191D-01, 0.6611506169154207D+00, &
    0.6670036283630431D+00, -0.4785541651013403D-01, &
    0.5349559346436485D+00, 0.9899002101148441D+00, &
    -0.3900488015542639D+00, -0.7458885499515072D+00, &
    0.5665446610649311D+00, 0.1548891409840480D-01, &
    -0.9844705836707888D+00, -0.6746117169036595D+00, &
    -0.1470918231599819D+00, -0.5584752636915873D+00, &
    -0.6804678115064715D+00, 0.8909927345534838D+00, &
    0.6374291217859047D+00, 0.9675265148508291D+00, &
    0.4420117190898502D+00, 0.7027112199253245D+00, &
    -0.3799739422082143D+00, 0.1856369603681191D+00, &
    0.7746845502717523D+00, 0.1761706438449975D+00, &
    0.9850881697540705D+00, -0.1129086405431629D+00, &
    0.2271646891547787D+00, -0.1127987048407091D+00, &
    -0.9200033438731134D+00, -0.6626998668546163D+00, &
    0.9270956556038703D+00, -0.2470253489012092D+00, &
    0.7729357655654772D+00, -0.7356540617546378D+00, &
    -0.3212372190294076D+00, 0.8111308146975724D+00, &
    0.6299555026169626D+00, 0.3951231967843170D-03, &
    -0.8706689354769571D+00, -0.9692856262585002D+00, &
    0.9756175749168980D+00, 0.6261197595970289D+00, &
    0.9652057112084984D+00, -0.9811255607149989D+00, &
    0.8079753140237501D+00, 0.4087152028895200D+00, &
    0.4607837944330232D+00, -0.9858814414729271D+00, &
    0.1870441924567301D-01, 0.9880110117128622D+00, &
    0.4089802625067766D+00, -0.9609137755818135D+00, &
    0.2225216531350550D+00, -0.9861887397735654D+00, &
    0.8950245843584548D+00, -0.6572193683282491D+00, &
    -0.9304667586038193D+00, 0.4475608353766626D+00, &
    -0.7058486792297883D+00, -0.9958976269497638D+00, &
    -0.8836382486319199D+00, -0.9857261377173898D+00, &
    -0.9951450397546436D+00, -0.5152481827199886D+00, &
    0.9915918444310511D+00, -0.4698538990980113D+00, &
    0.8727201920285046D+00, -0.5170681183537231D+00, &
    -0.9631512995815722D+00, 0.9953119825440425D+00, &
    0.4988843272874602D+00, 0.3927236168362344D+00 /
  data zs / &
    0.6938946186791187D+00, -0.5765923304339572D+00, &
    0.8051907870892863D+00, -0.8472224677887984D-01, &
    0.7318018811656403D-01, 0.5983817879906067D-01, &
    0.3384018846541420D+00, -0.4920664066906827D+00, &
    -0.7373417229245569D-01, 0.3303528888179985D+00, &
    0.1893355905396206D+00, -0.8380648944600999D+00, &
    0.9822767264128165D-01, -0.2594364391801900D+00, &
    0.1930055941915506D+00, -0.8498731486396089D+00, &
    -0.9480961804554179D-01, -0.8114075510008492D+00, &
    0.3445956986162864D+00, 0.6500251287969905D+00, &
    0.6911242761503570D+00, -0.5570201903906392D+00, &
    -0.4405333944883792D+00, -0.1725718027999678D+00, &
    -0.3345083448640582D+00, -0.2509356761788316D+00, &
    0.9317616130493932D+00, -0.9454774309944995D+00, &
    -0.7098026696648545D+00, -0.1134846343849767D+00, &
    -0.1315593065233067D+00, 0.2820827580654706D+00, &
    -0.8747191955455609D-01, -0.8330074038703938D+00, &
    -0.3660613547587568D+00, -0.9580088134576359D+00, &
    -0.4411464922737413D+00, 0.3949263385441928D+00, &
    -0.5640899102030010D+00, 0.2226335297251030D+00, &
    -0.9257134547555910D+00, 0.5715726078629155D+00, &
    -0.6345978485849848D+00, -0.9304344738485810D+00, &
    0.3590760844142656D+00, -0.4438907426169790D+00, &
    0.8671008381591335D+00, 0.6403756616117305D+00, &
    0.2778456794573282D+00, -0.7154487635298511D+00, &
    -0.5078493941700393D+00, -0.6363728062752509D+00, &
    -0.2513684642710016D+00, 0.8974010278661878D+00, &
    -0.9918409439634794D+00, -0.4534667592885467D+00, &
    0.4398586891128420D-01, -0.6130676703797119D+00, &
    -0.7477961790309134D+00, -0.7858333946639103D+00, &
    0.7974411188351147D+00, -0.9156598617795586D+00, &
    0.1715184891688095D+00, 0.1907051832001387D+00, &
    -0.7772414059750807D+00, -0.9276175191759890D-01, &
    -0.5405362134262204D+00, -0.1103237214049949D+00, &
    -0.3367426994768587D+00, 0.5093651265106862D+00, &
    -0.9492669502709199D+00, -0.9401434781115950D+00, &
    -0.5980941978161105D+00, -0.9625175885989743D+00, &
    0.1167123901439443D+00, 0.8885399133952907D+00, &
    0.5227273173456383D+00, -0.7706144348078099D+00, &
    0.5114844827530561D+00, 0.8060019555582266D+00, &
    0.8011735992610899D+00, 0.8195422038055056D+00, &
    0.9395527898900553D+00, 0.2015513588196933D+00, &
    -0.4186783562866535D-01, -0.5712296785602576D+00, &
    -0.9030794957455209D+00, -0.1456519314475667D+00, &
    0.5686925252996401D+00, 0.9600262609268753D+00, &
    -0.7217322859826423D+00, -0.5209570186170079D+00, &
    -0.9717413123988232D+00, 0.4303819820507280D+00, &
    -0.8147382303071704D+00, 0.6244560595709048D+00, &
    -0.1937189914196895D+00, -0.9083093391078847D+00, &
    -0.8428140765049459D+00, -0.9764924706512431D+00, &
    -0.9460095279803191D-01, -0.7952403780336077D+00, &
    0.2978906730661770D+00, 0.7125070355101365D+00, &
    0.9665991806647677D+00, 0.3839275268438931D+00, &
    -0.9734009359633906D+00, 0.6433849545611667D+00, &
    0.7568446904202943D+00, 0.2498470880629417D+00, &
    0.7852684316315199D+00, 0.3907525133961428D+00, &
    -0.4393237281615760D+00, 0.5260748276088058D-01, &
    0.6241597079858533D+00, 0.9654604035241454D+00, &
    0.7855716043610358D+00, 0.7924746367580968D+00, &
    -0.5575727107236519D+00, 0.9341485578568330D+00, &
    0.6036661107749994D+00, -0.6578753396306457D+00, &
    0.9656622221606493D+00, 0.9535888144290141D+00, &
    -0.3940334702415435D+00, 0.8677859635129408D+00, &
    0.4525286376195224D+00, 0.8943730814713799D+00, &
    0.9733506381066803D+00, -0.9666093420715280D+00, &
    -0.7599491907772208D+00, 0.2870616863165352D+00, &
    0.5428594142592057D+00, 0.9000765734384685D+00, &
    0.9346435403112985D+00, 0.7253765854245153D+00, &
    -0.8907140353727465D+00, 0.9838621660462418D+00, &
    0.9382671102570804D+00, 0.4142977100280188D+00, &
    -0.4245033033835321D+00, -0.3429310836114446D+00, &
    0.9810366085861507D+00, -0.2580286066432419D+00, &
    -0.9859800907904950D+00, 0.8105778659854276D+00, &
    -0.1748292017546381D+00, -0.8047213401011649D+00, &
    -0.9868201233515799D+00, -0.8647620143857092D+00, &
    -0.9830541038366302D+00, -0.9825555113262869D+00, &
    0.6976904274215842D+00, -0.2151083099716469D+00, &
    0.4203550437156745D-01, -0.9338232453847534D+00, &
    -0.6702199613543625D+00, 0.9924356236360254D+00, &
    -0.8478229607550496D+00, 0.9583412329573594D+00, &
    -0.7904934590950125D-01, 0.7258320418569250D+00, &
    0.1347146763176373D+00, 0.9283115095600205D+00, &
    0.7983610958403323D+00, 0.9982250382393918D+00, &
    -0.7346265359815700D+00, 0.9993883513767151D+00, &
    -0.9964956614827338D+00, -0.9525278817342810D+00, &
    0.6030693170605685D+00, 0.7499893148930252D+00 /
  data ws / &
    -0.1426541755470291D+00, 0.1385765810780117D-01, &
    0.1307994851680021D-01, 0.1073487590359840D-01, &
    0.1051260418442039D-01, 0.3275658093319255D-01, &
    0.1563154221413661D-01, 0.1207051149698361D-01, &
    0.3472763431163371D-01, 0.2607207312794260D-01, &
    0.7063494865440152D-02, 0.1455303522761317D-01, &
    0.3637446239280831D-01, 0.2741442134745744D-01, &
    0.1814853970042907D-01, 0.2211942357145999D-01, &
    0.2063146835573402D-01, 0.1187881032517930D-01, &
    0.3411740640767904D-01, 0.2044586424163537D-01, &
    0.1584398903577958D-01, 0.2546900742398995D-01, &
    0.1514164315096591D-01, 0.1383700180253657D-01, &
    0.3244335479410856D-01, 0.2480839462235662D-01, &
    0.1901942550392541D-02, 0.8435911241596891D-02, &
    0.2592855621096610D-01, 0.2917638594610470D-01, &
    0.4254452270715581D-01, 0.2672040615070632D-01, &
    0.1679948502767675D-01, 0.1441579204032182D-01, &
    0.4055353005194201D-01, 0.3562579203350458D-02, &
    0.1304869501588879D-01, 0.7974389607375001D-02, &
    0.1942169211659009D-01, 0.7696214255731893D-02, &
    0.9786582979889373D-02, 0.2656456655588142D-01, &
    0.3887064238866873D-01, 0.4865147372961578D-02, &
    0.7182928711371533D-02, 0.2992836230232769D-01, &
    0.1246769069332907D-02, 0.1018121368113786D-01, &
    0.1583310102000952D-01, 0.3101719294140454D-01, &
    0.3352533262979018D-01, 0.1304614239329544D-01, &
    0.2424339545109358D-01, 0.2062227856017715D-01, &
    0.4634332034618291D-02, 0.1316721066365703D-01, &
    0.4197768965655505D-01, 0.1445189210678322D-01, &
    0.2742773234129495D-01, 0.1147786426088701D-01, &
    0.1051388351726134D-01, 0.1177681391912994D-01, &
    0.1280321776997680D-01, 0.2638280742080748D-01, &
    0.1039146162515972D-01, 0.6350797568985297D-02, &
    0.3731595769989320D-02, 0.4022230391599942D-02, &
    0.3700138698657982D-01, 0.1223831728952482D-01, &
    0.1361018792408060D-01, 0.2432559937042062D-02, &
    0.3173506278445155D-01, 0.5299185434865185D-02, &
    0.4119660596888559D-01, 0.8140787806672345D-02, &
    0.4245764049999567D-01, 0.2966712362252400D-01, &
    0.5251158142618067D-02, 0.3117784323015366D-02, &
    0.1887240736415798D-01, 0.1982870468110188D-01, &
    0.1525468960328201D-01, 0.2145937561316863D-01, &
    0.2757411556050665D-01, 0.1580892051031026D-01, &
    0.2339199782020639D-01, 0.1652864001989000D-01, &
    0.2227125289030323D-01, 0.4542047728295454D-02, &
    0.1029753747302055D-01, 0.7588833799248887D-02, &
    0.7729962070932306D-02, 0.1686200103735939D-01, &
    0.9855191228454099D-02, 0.2298392421656214D-01, &
    0.5390281878584920D-01, 0.1777695356252309D-01, &
    0.1543307662800650D-01, 0.1565350686857245D-02, &
    0.2990218331164426D-01, 0.2492829949352890D-01, &
    0.4068595698602832D-01, 0.3904492542455568D-01, &
    0.1145081666124139D-01, 0.7199499019479311D-02, &
    0.1180596844175838D-01, 0.8490203829980697D-02, &
    0.2407429627173780D-01, 0.6013060579704863D-01, &
    0.4782368835976528D-02, 0.4070082427805708D-01, &
    0.5421074129521822D-01, 0.3495817844335809D-01, &
    0.1411530671886532D-01, 0.5360156659230995D-02, &
    0.1684523030402797D-01, 0.8957506702452017D-02, &
    0.2843883212961609D-01, 0.1548562016507650D-01, &
    0.4755338701620224D-01, 0.1061956205236735D-01, &
    0.4143328303679084D-02, 0.1091266243253667D-01, &
    0.7928114591801557D-02, 0.2853726563554603D-01, &
    0.3789221238377655D-01, 0.1479871521595558D-01, &
    0.5265072839453472D-02, 0.8371632675069558D-02, &
    0.9778033410801315D-02, 0.1741500947112345D-01, &
    0.1416545817648641D-01, 0.1911128041899456D-01, &
    0.4409869231823535D-02, 0.2671133974518937D-01, &
    0.8042675590771235D-02, 0.1102479574395968D-01, &
    0.5810239704806290D-02, 0.7613319884605023D-02, &
    0.6957655736787461D-02, 0.2091424497221789D-01, &
    0.2998965152610065D-02, 0.6589150225887293D-02, &
    0.4511407449002006D-02, 0.6488450322177047D-02, &
    0.8640589231658198D-02, 0.5930519400069490D-02, &
    0.4449535891375782D-02, 0.4479599530453893D-02, &
    0.1130073799626979D-01, 0.1782480610982645D-02, &
    0.9295402400959387D-02, 0.9494483239679656D-02, &
    0.3985977390728825D-02, 0.2723087629850607D-02, &
    0.2577391123867757D-02, 0.3091874244052285D-02, &
    0.3705458814905999D-02, 0.2140475474717141D-02, &
    0.4774963198255120D-02, 0.1630610059874353D-02, &
    0.9085452239351545D-02, 0.2955971697674354D-02, &
    0.2082908274049303D-02, 0.5720118558010348D-02, &
    0.2931930843304469D-02, 0.5718185266004892D-02, &
    0.2289842864717349D-02, 0.6408796456581044D-03, &
    0.1069581923634952D+00, 0.1069506278589204D+00 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  w(1:n) = ws(1:n)

  return
end
subroutine rule15 ( n, x, w )

!*****************************************************************************80
!
!! RULE15 returns the rule of degree 15.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 July 2014
!
!  Author:
!
!    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
!    This FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Hong Xiao, Zydrunas Gimbutas,
!    A numerical algorithm for the construction of efficient quadrature
!    rules in two and higher dimensions,
!    Computers and Mathematics with Applications,
!    Volume 59, 2010, pages 663-676.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(3,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 190 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(3,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)
  real ( kind = 8 ) zs(ns)

  save xs
  save ys
  save zs
  save ws

  data xs / &
    0.4846214646897683D+00, 0.6027166661210369D-01, &
    0.5119120893947191D+00, -0.5797527815798613D+00, &
    0.8916317278515248D+00, 0.4427375637743738D+00, &
    -0.2409001206926237D+00, 0.4213881124359606D+00, &
    0.2454110415378226D+00, -0.1642469924955429D+00, &
    -0.5907939646559498D+00, -0.6671386201069288D+00, &
    0.7552290798850946D+00, 0.1001864121384495D+00, &
    -0.7219847932977880D+00, 0.2982096063117719D+00, &
    0.5073944886321697D+00, -0.4863692703613542D+00, &
    0.4639404445534597D+00, -0.8081387191848195D+00, &
    -0.5896154562963155D+00, 0.1349771790076746D+00, &
    -0.1459114981397958D+00, -0.7771044530879095D+00, &
    0.3049106464545794D+00, 0.7478398889947702D+00, &
    0.2593964431357709D+00, -0.8826393735985893D+00, &
    0.5355784681079047D+00, -0.1464672879292506D+00, &
    0.6066659900076390D+00, -0.8588774763360313D+00, &
    -0.9697378209073355D+00, -0.3207448401664478D+00, &
    0.3238825883494653D+00, -0.8911881463557618D+00, &
    0.9588421432293531D+00, -0.9459659824876482D-01, &
    0.4326888655727574D+00, -0.4031429838762605D+00, &
    -0.4784772616280669D+00, 0.8992518592510214D+00, &
    0.6586422425595307D+00, 0.8204427703547663D+00, &
    -0.5614110159343418D+00, 0.9520903109905504D-01, &
    0.5640256443183821D+00, -0.8042276926515493D+00, &
    0.6821201856074234D+00, -0.7682858721251195D+00, &
    0.7082541697630407D+00, -0.4618970778119202D+00, &
    -0.2168388877255139D+00, 0.5371020397039006D+00, &
    0.8885379409916848D+00, 0.6251053221154703D+00, &
    0.8755744976159847D+00, -0.1289396220206777D+00, &
    0.9646166796039118D+00, 0.2607898402928713D+00, &
    0.9703940236997658D+00, 0.7108751014443095D-01, &
    0.7712967479205184D+00, 0.8842270989720199D+00, &
    0.7991197473160693D+00, -0.3410291594283488D+00, &
    -0.4192756150578085D+00, 0.7230818664415763D+00, &
    0.1436938638381775D+00, -0.9757899278543728D+00, &
    0.8447822406339708D+00, 0.9613637788842541D+00, &
    0.8507427476805113D+00, -0.8848833562540662D+00, &
    0.9720070913947075D+00, -0.6259228134698737D+00, &
    0.9441521606893316D+00, 0.2886043547146998D+00, &
    0.9428658265051933D+00, 0.9499838153205579D+00, &
    -0.1175912491528422D+00, -0.9810321115887223D+00, &
    0.9305760666668829D+00, -0.9783274226607617D+00, &
    -0.9834890551889414D+00, 0.4453699774671322D-01, &
    -0.5306871050560035D+00, 0.9868369576014261D+00, &
    -0.5232830891127319D+00, -0.8918143354641236D+00, &
    0.8676179401449021D+00, 0.9964872079697851D+00, &
    0.9918319243897599D+00, 0.7865264660879382D+00, &
    -0.9241660476457195D-01, -0.4846214646897686D+00, &
    -0.6027166661210399D-01, -0.5119120893947190D+00, &
    0.5797527815798613D+00, -0.8916317278515249D+00, &
    -0.4427375637743741D+00, 0.2409001206926235D+00, &
    -0.4213881124359606D+00, -0.2454110415378225D+00, &
    0.1642469924955428D+00, 0.5907939646559499D+00, &
    0.6671386201069287D+00, -0.7552290798850947D+00, &
    -0.1001864121384497D+00, 0.7219847932977881D+00, &
    -0.2982096063117720D+00, -0.5073944886321696D+00, &
    0.4863692703613542D+00, -0.4639404445534597D+00, &
    0.8081387191848196D+00, 0.5896154562963155D+00, &
    -0.1349771790076746D+00, 0.1459114981397958D+00, &
    0.7771044530879095D+00, -0.3049106464545795D+00, &
    -0.7478398889947702D+00, -0.2593964431357710D+00, &
    0.8826393735985893D+00, -0.5355784681079047D+00, &
    0.1464672879292507D+00, -0.6066659900076391D+00, &
    0.8588774763360314D+00, 0.9697378209073355D+00, &
    0.3207448401664478D+00, -0.3238825883494653D+00, &
    0.8911881463557617D+00, -0.9588421432293531D+00, &
    0.9459659824876483D-01, -0.4326888655727574D+00, &
    0.4031429838762604D+00, 0.4784772616280668D+00, &
    -0.8992518592510215D+00, -0.6586422425595307D+00, &
    -0.8204427703547664D+00, 0.5614110159343418D+00, &
    -0.9520903109905507D-01, -0.5640256443183821D+00, &
    0.8042276926515494D+00, -0.6821201856074234D+00, &
    0.7682858721251195D+00, -0.7082541697630407D+00, &
    0.4618970778119202D+00, 0.2168388877255139D+00, &
    -0.5371020397039005D+00, -0.8885379409916848D+00, &
    -0.6251053221154704D+00, -0.8755744976159846D+00, &
    0.1289396220206776D+00, -0.9646166796039118D+00, &
    -0.2607898402928713D+00, -0.9703940236997658D+00, &
    -0.7108751014443089D-01, -0.7712967479205184D+00, &
    -0.8842270989720198D+00, -0.7991197473160693D+00, &
    0.3410291594283487D+00, 0.4192756150578084D+00, &
    -0.7230818664415765D+00, -0.1436938638381774D+00, &
    0.9757899278543728D+00, -0.8447822406339707D+00, &
    -0.9613637788842542D+00, -0.8507427476805113D+00, &
    0.8848833562540663D+00, -0.9720070913947076D+00, &
    0.6259228134698737D+00, -0.9441521606893316D+00, &
    -0.2886043547146998D+00, -0.9428658265051933D+00, &
    -0.9499838153205579D+00, 0.1175912491528422D+00, &
    0.9810321115887223D+00, -0.9305760666668829D+00, &
    0.9783274226607617D+00, 0.9834890551889414D+00, &
    -0.4453699774671330D-01, 0.5306871050560035D+00, &
    -0.9868369576014261D+00, 0.5232830891127319D+00, &
    0.8918143354641237D+00, -0.8676179401449020D+00, &
    -0.9964872079697851D+00, -0.9918319243897600D+00, &
    -0.7865264660879382D+00, 0.9241660476457186D-01 /
  data ys / &
    -0.2262155685815833D+00, -0.5802224500744104D+00, &
    -0.8812418910175070D+00, 0.4577182827511190D-01, &
    0.7310521233845833D+00, 0.4830906487655276D+00, &
    -0.8061293440666314D+00, -0.1078387681721650D+00, &
    0.8648936466868999D+00, 0.1116488353049886D+00, &
    -0.9117169599889896D+00, 0.7671850694134570D+00, &
    0.8098232031170816D+00, 0.9416109016304105D+00, &
    -0.8571607992505791D+00, -0.8294951885681802D+00, &
    -0.3287584215285467D-01, -0.9716511178926364D+00, &
    0.4508573465614439D+00, -0.9829067455694369D+00, &
    -0.3886494734052406D+00, 0.6210687867431628D+00, &
    0.7058206696430964D+00, 0.8874742900432545D+00, &
    0.2936260801274535D-01, 0.4728249466868350D+00, &
    -0.7999771197371979D+00, 0.9406572379268412D-01, &
    -0.4746762454784017D+00, 0.5158509951163104D+00, &
    -0.8256099261094548D+00, -0.5791703340444312D+00, &
    0.4466546017661202D+00, -0.1927320854548640D+00, &
    -0.4345261425945900D+00, 0.5425166397866776D-02, &
    0.9375941025615940D+00, -0.3420778054235573D+00, &
    0.7075968971274725D+00, 0.6496964078624228D+00, &
    0.4004462354823037D+00, -0.6968996840182875D+00, &
    0.3055994655170601D+00, -0.5169213379049986D+00, &
    0.9549786512838463D+00, -0.8791296893067777D+00, &
    0.8303000567130658D+00, -0.6377483111957468D+00, &
    0.8739025562234195D+00, -0.1693477428011365D+00, &
    -0.2351350601654628D+00, -0.6299185210533546D+00, &
    0.1604152098962463D+00, -0.5905825901125751D+00, &
    0.1734591472408230D+00, 0.2417993452840939D+00, &
    -0.7167382075250317D+00, -0.5500881365309197D+00, &
    -0.2400335193040073D+00, 0.7704776594613698D+00, &
    -0.5549501648642681D+00, 0.2871796808122397D+00, &
    -0.2817721114790634D+00, 0.9549722793117502D+00, &
    -0.9499810867427769D+00, 0.9910163892213160D+00, &
    -0.9801994414073382D+00, -0.4857741672686406D+00, &
    -0.9028324174014650D+00, -0.7689895270308303D+00, &
    -0.8885921033548654D+00, 0.2255470425271027D+00, &
    -0.7650697965238176D-01, 0.9735656776897391D+00, &
    -0.2305869558790821D+00, 0.7981924334040106D+00, &
    -0.6544414972640588D+00, -0.9790786374374271D+00, &
    0.9251057242786117D+00, 0.6489915714062549D+00, &
    -0.9627047612899647D+00, 0.9597706404726861D+00, &
    0.5373877582566533D+00, 0.8266106540930876D+00, &
    -0.8547964831867243D+00, 0.9558026916663981D+00, &
    0.9856879028440860D+00, -0.9065724188604554D+00, &
    -0.9845747454374670D+00, 0.9891294476252082D+00, &
    0.9881191368012432D+00, 0.3020236952873330D+00, &
    0.3856116113348522D+00, 0.7292572061308893D+00, &
    -0.1882646447022738D+00, 0.2262155685815840D+00, &
    0.5802224500744105D+00, 0.8812418910175072D+00, &
    -0.4577182827511186D-01, -0.7310521233845831D+00, &
    -0.4830906487655275D+00, 0.8061293440666314D+00, &
    0.1078387681721650D+00, -0.8648936466868998D+00, &
    -0.1116488353049886D+00, 0.9117169599889896D+00, &
    -0.7671850694134570D+00, -0.8098232031170816D+00, &
    -0.9416109016304105D+00, 0.8571607992505792D+00, &
    0.8294951885681802D+00, 0.3287584215285466D-01, &
    0.9716511178926363D+00, -0.4508573465614438D+00, &
    0.9829067455694369D+00, 0.3886494734052407D+00, &
    -0.6210687867431628D+00, -0.7058206696430964D+00, &
    -0.8874742900432544D+00, -0.2936260801274522D-01, &
    -0.4728249466868351D+00, 0.7999771197371979D+00, &
    -0.9406572379268432D-01, 0.4746762454784017D+00, &
    -0.5158509951163104D+00, 0.8256099261094548D+00, &
    0.5791703340444312D+00, -0.4466546017661200D+00, &
    0.1927320854548639D+00, 0.4345261425945901D+00, &
    -0.5425166397866769D-02, -0.9375941025615940D+00, &
    0.3420778054235572D+00, -0.7075968971274725D+00, &
    -0.6496964078624228D+00, -0.4004462354823038D+00, &
    0.6968996840182876D+00, -0.3055994655170602D+00, &
    0.5169213379049985D+00, -0.9549786512838463D+00, &
    0.8791296893067777D+00, -0.8303000567130657D+00, &
    0.6377483111957469D+00, -0.8739025562234195D+00, &
    0.1693477428011367D+00, 0.2351350601654628D+00, &
    0.6299185210533547D+00, -0.1604152098962464D+00, &
    0.5905825901125752D+00, -0.1734591472408231D+00, &
    -0.2417993452840938D+00, 0.7167382075250318D+00, &
    0.5500881365309197D+00, 0.2400335193040072D+00, &
    -0.7704776594613698D+00, 0.5549501648642682D+00, &
    -0.2871796808122396D+00, 0.2817721114790634D+00, &
    -0.9549722793117502D+00, 0.9499810867427769D+00, &
    -0.9910163892213160D+00, 0.9801994414073382D+00, &
    0.4857741672686406D+00, 0.9028324174014649D+00, &
    0.7689895270308303D+00, 0.8885921033548654D+00, &
    -0.2255470425271028D+00, 0.7650697965238172D-01, &
    -0.9735656776897391D+00, 0.2305869558790820D+00, &
    -0.7981924334040106D+00, 0.6544414972640588D+00, &
    0.9790786374374271D+00, -0.9251057242786117D+00, &
    -0.6489915714062550D+00, 0.9627047612899647D+00, &
    -0.9597706404726861D+00, -0.5373877582566532D+00, &
    -0.8266106540930876D+00, 0.8547964831867243D+00, &
    -0.9558026916663980D+00, -0.9856879028440859D+00, &
    0.9065724188604554D+00, 0.9845747454374671D+00, &
    -0.9891294476252082D+00, -0.9881191368012432D+00, &
    -0.3020236952873331D+00, -0.3856116113348522D+00, &
    -0.7292572061308894D+00, 0.1882646447022739D+00 /
  data zs / &
    0.9718923364663833D+00, -0.6995279408119103D+00, &
    0.4077821712986755D+00, -0.1984488505968536D+00, &
    -0.1635978697790265D+00, -0.9519012083224356D+00, &
    -0.7709973566567310D+00, 0.6575891838559990D+00, &
    -0.2431469413756804D+00, -0.2154126099125328D+00, &
    0.2917708722416409D+00, -0.7837093593401955D+00, &
    0.5503667466636896D-01, 0.6034320247288605D+00, &
    -0.5348830267053428D+00, 0.6967890074634113D+00, &
    -0.6004372124809728D+00, -0.7978753021283712D+00, &
    0.7111633203919522D+00, -0.4796592851863407D+00, &
    -0.1066936069842310D+00, -0.4937724895804262D-02, &
    -0.3617523885223834D+00, 0.7864082151686236D+00, &
    0.8896593937798930D+00, -0.3003206340764916D+00, &
    -0.8604298301675264D+00, -0.9764052940319004D+00, &
    0.4627660680406495D+00, -0.8466728944778892D+00, &
    -0.9693428983943551D+00, 0.7807520536361380D+00, &
    0.2410080019231437D+00, 0.2275963934164933D+00, &
    -0.1058345123272911D+00, -0.4676766976465098D-01, &
    0.1025372637039018D+00, 0.6347152609726029D+00, &
    0.3752067881462010D+00, 0.5361771132313945D+00, &
    0.9695076036670979D+00, 0.6398892617052058D+00, &
    0.9539776797271410D+00, 0.1946562772645561D+00, &
    0.5161190440947852D+00, -0.2830148609612029D+00, &
    0.9517662443841286D+00, -0.7967109780188084D+00, &
    -0.7304370458992153D+00, -0.5338593513474772D+00, &
    -0.3179475898860780D+00, 0.5547973598962450D+00, &
    0.8313518081338683D+00, 0.9654245596905445D+00, &
    -0.5628516000227574D+00, -0.8613302773849665D+00, &
    -0.4483881659919932D+00, -0.9517697705546500D+00, &
    -0.7234814778587770D+00, -0.8875078228929659D+00, &
    0.9243612035652934D+00, 0.4601935017159294D+00, &
    0.8090143745218911D+00, 0.9163662035802967D+00, &
    0.4382382798746217D+00, 0.8459991721751295D+00, &
    -0.1366146439527482D+00, -0.7720184760904137D+00, &
    0.9707987948940664D+00, 0.5206831670219514D+00, &
    0.9588210246069581D+00, 0.8185288301633218D+00, &
    -0.9577089524402139D+00, 0.1925548617062123D+00, &
    0.4997201390064559D+00, 0.5406623725365253D-01, &
    -0.9375810397798050D+00, 0.1306044624286102D+00, &
    -0.8929122835431057D+00, 0.9831724010468225D+00, &
    0.6385058885805324D+00, -0.7720170702032852D+00, &
    0.3549666743399563D+00, -0.1714024163539602D+00, &
    -0.6784170912845590D+00, 0.9880880116453774D+00, &
    -0.8468765296700308D+00, -0.6791166853273773D+00, &
    0.9378710595411145D+00, 0.9586435791263295D+00, &
    -0.4583535837605982D+00, -0.1539477252636465D+00, &
    -0.9251749829870214D+00, -0.9949152686131382D+00, &
    0.9985713487179375D+00, -0.9718923364663833D+00, &
    0.6995279408119103D+00, -0.4077821712986756D+00, &
    0.1984488505968536D+00, 0.1635978697790266D+00, &
    0.9519012083224355D+00, 0.7709973566567310D+00, &
    -0.6575891838559990D+00, 0.2431469413756804D+00, &
    0.2154126099125329D+00, -0.2917708722416409D+00, &
    0.7837093593401955D+00, -0.5503667466636887D-01, &
    -0.6034320247288605D+00, 0.5348830267053428D+00, &
    -0.6967890074634113D+00, 0.6004372124809728D+00, &
    0.7978753021283711D+00, -0.7111633203919521D+00, &
    0.4796592851863406D+00, 0.1066936069842310D+00, &
    0.4937724895804275D-02, 0.3617523885223834D+00, &
    -0.7864082151686236D+00, -0.8896593937798931D+00, &
    0.3003206340764915D+00, 0.8604298301675265D+00, &
    0.9764052940319005D+00, -0.4627660680406495D+00, &
    0.8466728944778892D+00, 0.9693428983943552D+00, &
    -0.7807520536361380D+00, -0.2410080019231437D+00, &
    -0.2275963934164933D+00, 0.1058345123272911D+00, &
    0.4676766976465104D-01, -0.1025372637039019D+00, &
    -0.6347152609726029D+00, -0.3752067881462010D+00, &
    -0.5361771132313945D+00, -0.9695076036670979D+00, &
    -0.6398892617052058D+00, -0.9539776797271410D+00, &
    -0.1946562772645561D+00, -0.5161190440947853D+00, &
    0.2830148609612028D+00, -0.9517662443841286D+00, &
    0.7967109780188085D+00, 0.7304370458992153D+00, &
    0.5338593513474772D+00, 0.3179475898860781D+00, &
    -0.5547973598962450D+00, -0.8313518081338683D+00, &
    -0.9654245596905445D+00, 0.5628516000227572D+00, &
    0.8613302773849664D+00, 0.4483881659919933D+00, &
    0.9517697705546500D+00, 0.7234814778587769D+00, &
    0.8875078228929659D+00, -0.9243612035652934D+00, &
    -0.4601935017159294D+00, -0.8090143745218911D+00, &
    -0.9163662035802967D+00, -0.4382382798746217D+00, &
    -0.8459991721751295D+00, 0.1366146439527482D+00, &
    0.7720184760904137D+00, -0.9707987948940665D+00, &
    -0.5206831670219514D+00, -0.9588210246069581D+00, &
    -0.8185288301633218D+00, 0.9577089524402139D+00, &
    -0.1925548617062122D+00, -0.4997201390064560D+00, &
    -0.5406623725365260D-01, 0.9375810397798049D+00, &
    -0.1306044624286103D+00, 0.8929122835431057D+00, &
    -0.9831724010468225D+00, -0.6385058885805324D+00, &
    0.7720170702032852D+00, -0.3549666743399564D+00, &
    0.1714024163539601D+00, 0.6784170912845590D+00, &
    -0.9880880116453774D+00, 0.8468765296700308D+00, &
    0.6791166853273775D+00, -0.9378710595411145D+00, &
    -0.9586435791263295D+00, 0.4583535837605982D+00, &
    0.1539477252636465D+00, 0.9251749829870214D+00, &
    0.9949152686131382D+00, -0.9985713487179375D+00 /
  data ws / &
    0.2931082855526895D-02, 0.1466168295291819D-01, &
    0.1008190603381851D-01, 0.2521840249289902D-01, &
    0.9545986541148931D-02, 0.7815725861454997D-02, &
    0.1225157612225792D-01, 0.2516512639883486D-01, &
    0.1749166437590727D-01, 0.4056629885298555D-01, &
    0.1199350194114567D-01, 0.1133390863336471D-01, &
    0.1485090749543295D-01, 0.1026773216326104D-01, &
    0.1201022409690237D-01, 0.1547758419389476D-01, &
    0.2898163426866567D-01, 0.5077671719424524D-02, &
    0.2462022242995652D-01, 0.3796424546988349D-02, &
    0.3310665553884919D-01, 0.3508140615002558D-01, &
    0.2950704004490954D-01, 0.8017748658106175D-02, &
    0.1975278275621659D-01, 0.2559496685146834D-01, &
    0.1349736154064143D-01, 0.4377789154192862D-02, &
    0.3252004086526810D-01, 0.2138516935436983D-01, &
    0.5015100019990201D-02, 0.1268265369185493D-01, &
    0.9980860281711945D-02, 0.4271671806230670D-01, &
    0.4274198226264674D-01, 0.2231057111860769D-01, &
    0.4634811584165995D-02, 0.3628925840326913D-01, &
    0.2963883283828190D-01, 0.3004053280506377D-01, &
    0.9656867842652010D-02, 0.1208027996271507D-01, &
    0.1067097545113014D-01, 0.2483163417257641D-01, &
    0.1052923525628832D-01, 0.2351898637367898D-01, &
    0.7064042544802274D-02, 0.1458074471394978D-01, &
    0.1255548203713305D-01, 0.2826856991152390D-01, &
    0.3484550434776072D-01, 0.3029815093584674D-01, &
    0.2852474545169975D-01, 0.9146009437075924D-02, &
    0.1990115803594484D-01, 0.2069177392023117D-01, &
    0.1613360621469677D-01, 0.1339344756044318D-01, &
    0.9274781087469087D-02, 0.1530433781694090D-01, &
    0.3917613624484169D-02, 0.4743890758461208D-01, &
    0.2001420743439168D-01, 0.2832855798485957D-02, &
    0.9111027190743101D-02, 0.3380728915063295D-02, &
    0.9357526881990973D-02, 0.2181586678118927D-01, &
    0.5497095242202157D-02, 0.6401916148758926D-02, &
    0.3861833303734157D-02, 0.8708380742935694D-02, &
    0.8616911070460580D-02, 0.5860715399693166D-02, &
    0.1134352478246400D-01, 0.2833705081651916D-01, &
    0.4870087478700985D-02, 0.1085506021481164D-01, &
    0.3085543406158564D-02, 0.2320131607643815D-02, &
    0.1217435492763373D-01, 0.1876868717373962D-02, &
    0.1764889584855365D-01, 0.6738482260511419D-02, &
    0.4000917701843583D-02, 0.2530717864603938D-02, &
    0.4454840968844286D-02, 0.2917862788830831D-02, &
    0.3031055423950877D-02, 0.1091305108499325D-02, &
    0.4268181631337476D-02, 0.6826137445231240D-02, &
    0.3247017230887978D-02, 0.3899933571265500D-02, &
    0.7578127425388513D-02, 0.2931082855526887D-02, &
    0.1466168295291819D-01, 0.1008190603381851D-01, &
    0.2521840249289902D-01, 0.9545986541148931D-02, &
    0.7815725861455011D-02, 0.1225157612225792D-01, &
    0.2516512639883484D-01, 0.1749166437590725D-01, &
    0.4056629885298554D-01, 0.1199350194114567D-01, &
    0.1133390863336472D-01, 0.1485090749543296D-01, &
    0.1026773216326105D-01, 0.1201022409690238D-01, &
    0.1547758419389476D-01, 0.2898163426866567D-01, &
    0.5077671719424526D-02, 0.2462022242995652D-01, &
    0.3796424546988351D-02, 0.3310665553884920D-01, &
    0.3508140615002557D-01, 0.2950704004490954D-01, &
    0.8017748658106186D-02, 0.1975278275621657D-01, &
    0.2559496685146833D-01, 0.1349736154064142D-01, &
    0.4377789154192863D-02, 0.3252004086526811D-01, &
    0.2138516935436982D-01, 0.5015100019990189D-02, &
    0.1268265369185493D-01, 0.9980860281711927D-02, &
    0.4271671806230667D-01, 0.4274198226264675D-01, &
    0.2231057111860768D-01, 0.4634811584165991D-02, &
    0.3628925840326913D-01, 0.2963883283828189D-01, &
    0.3004053280506377D-01, 0.9656867842652003D-02, &
    0.1208027996271506D-01, 0.1067097545113014D-01, &
    0.2483163417257640D-01, 0.1052923525628832D-01, &
    0.2351898637367896D-01, 0.7064042544802277D-02, &
    0.1458074471394977D-01, 0.1255548203713306D-01, &
    0.2826856991152389D-01, 0.3484550434776074D-01, &
    0.3029815093584674D-01, 0.2852474545169975D-01, &
    0.9146009437075919D-02, 0.1990115803594483D-01, &
    0.2069177392023119D-01, 0.1613360621469677D-01, &
    0.1339344756044317D-01, 0.9274781087469082D-02, &
    0.1530433781694089D-01, 0.3917613624484167D-02, &
    0.4743890758461208D-01, 0.2001420743439166D-01, &
    0.2832855798485960D-02, 0.9111027190743106D-02, &
    0.3380728915063295D-02, 0.9357526881990973D-02, &
    0.2181586678118927D-01, 0.5497095242202148D-02, &
    0.6401916148758928D-02, 0.3861833303734160D-02, &
    0.8708380742935682D-02, 0.8616911070460571D-02, &
    0.5860715399693163D-02, 0.1134352478246400D-01, &
    0.2833705081651915D-01, 0.4870087478700985D-02, &
    0.1085506021481165D-01, 0.3085543406158568D-02, &
    0.2320131607643819D-02, 0.1217435492763373D-01, &
    0.1876868717373960D-02, 0.1764889584855365D-01, &
    0.6738482260511409D-02, 0.4000917701843592D-02, &
    0.2530717864603941D-02, 0.4454840968844280D-02, &
    0.2917862788830830D-02, 0.3031055423950871D-02, &
    0.1091305108499324D-02, 0.4268181631337478D-02, &
    0.6826137445231243D-02, 0.3247017230887967D-02, &
    0.3899933571265500D-02, 0.7578127425388515D-02 /

  call r8mat_row_copy ( 3, n, 1, xs, x )
  call r8mat_row_copy ( 3, n, 2, ys, x )
  call r8mat_row_copy ( 3, n, 3, zs, x )
  w(1:n) = ws(1:n)

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
