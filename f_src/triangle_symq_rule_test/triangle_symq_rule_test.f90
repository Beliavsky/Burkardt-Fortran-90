program main

!*****************************************************************************80
!
!! MAIN is the main program for TRIANGLE_SYMQ_RULE_TEST.
!
!  Discussion:
!
!    TRIANGLE_SYMQ_RULE_TEST tests the TRIANGLE_SYMQ_RULE library.
!
!  Licensing:
!
!    This code is distributed under the GNU GPL license.
!
!  Modified:
!
!    26 June 2014
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
  implicit none

  integer ( kind = 4 ) degree
  character * ( 255 ) header
  integer ( kind = 4 ) itype
  integer ( kind = 4 ) numnodes
  real ( kind = 8 ) vert1(2)
  real ( kind = 8 ) vert2(2)
  real ( kind = 8 ) vert3(2)

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRIANGLE_SYMQ_RULE_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the TRIANGLE_SYMQ_RULE library.'

  call test01 ( )

  do itype = 0, 2

    if ( itype == 0 ) then

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) '  Region is user-defined triangle.'
      vert1(1) = 1.0D+00
      vert1(2) = 0.0D+00
      vert2(1) = 4.0D+00
      vert2(2) = 4.0D+00
      vert3(1) = 0.0D+00
      vert3(2) = 3.0D+00
      header = 'user08'
      degree = 8

    else if ( itype == 1 ) then

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) &
        '  Region is standard equilateral triangle.'
      vert1(1) = -1.0D+00
      vert1(2) = -1.0D+00 / sqrt ( 3.0D+00 )
      vert2(1) = +1.0D+00
      vert2(2) = -1.0D+00 / sqrt ( 3.0D+00 )
      vert3(1) =  0.0D+00
      vert3(2) =  2.0D+00 / sqrt ( 3.0D+00 )
      header = 'equi08'
      degree = 8

    else if ( itype == 2 ) then

      write ( *, '(a)' ) ''
      write ( *, '(a)' ) &
        '  Region is the simplex (0,0),(1,0),(0,1).'
      vert1(1) = 0.0D+00
      vert1(2) = 0.0D+00
      vert2(1) = 1.0D+00
      vert2(2) = 0.0D+00
      vert3(1) = 0.0D+00
      vert3(2) = 1.0D+00
      header = 'simp08'
      degree = 8

    end if

    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) '  Triangle:'
    write ( *, '(a)' ) ' '
    write ( *, '(2x,g14.6,2x,g14.6)' ) vert1(1:2)
    write ( *, '(2x,g14.6,2x,g14.6)' ) vert2(1:2)
    write ( *, '(2x,g14.6,2x,g14.6)' ) vert3(1:2)
!
!  Determine the size of the rule.
!
    call rule_full_size ( degree, numnodes )
!
!  Retrieve a rule and print it.
!
    call test02 ( degree, numnodes, vert1, vert2, vert3 )
!
!  Get a rule, and write data files that gnuplot can use to plot the points.
!
    call test03 ( degree, numnodes, vert1, vert2, vert3, header )

    call test04 ( degree, numnodes, vert1, vert2, vert3, header )

    call test05 ( degree, numnodes, vert1, vert2, vert3 )

  end do
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TRIANGLE_SYMQ_RULE_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( )

!*****************************************************************************80
!
!! TEST01 tests TRIANGLE_TO_SIMPLEX, TRIANGLE_TO_REF, REF_TO_TRIANGLE, SIMPLEX_TO_TRIANGLE.
!
!  Licensing:
!
!    This code is distributed under the GNU GPL license.
!
!  Modified:
!
!    21 June 2014
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer ( kind = 4 ) i
  real ( kind = 8 ) rp1(2)
  real ( kind = 8 ) rv1(2)
  real ( kind = 8 ) rv2(2)
  real ( kind = 8 ) rv3(2)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) sp1(2)
  real ( kind = 8 ) sp2(2)
  real ( kind = 8 ) sv1(2)
  real ( kind = 8 ) sv2(2)
  real ( kind = 8 ) sv3(2)
  real ( kind = 8 ) tp1(2)
  real ( kind = 8 ) tp2(2)
  real ( kind = 8 ) tv1(2)
  real ( kind = 8 ) tv2(2)
  real ( kind = 8 ) tv3(2)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Map points from one triangle to another.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  R = reference triangle'
  write ( *, '(a)' ) '  S = simplex'
  write ( *, '(a)' ) '  T = user-defined triangle.'
  write ( *, '(a)' ) '  REF_TO_TRIANGLE:     R => T'
  write ( *, '(a)' ) '  SIMPLEX_TO_TRIANGLE: S => T'
  write ( *, '(a)' ) '  TRIANGLE_TO_REF:     T => R'
  write ( *, '(a)' ) '  TRIANGLE_TO_SIMPLEX: T => S'
!
!  Reference triangle
!
  rv1(1) = -1.0D+00
  rv1(2) = -1.0D+00 / sqrt ( 3.0D+00 )
  rv2(1) = +1.0D+00
  rv2(2) = -1.0D+00 / sqrt ( 3.0D+00 )
  rv3(1) =  0.0D+00
  rv3(2) =  2.0D+00 / sqrt ( 3.0D+00 )
!
!  Simplex
!
  sv1(1) = 0.0D+00
  sv1(2) = 0.0D+00
  sv2(1) = 1.0D+00
  sv2(2) = 0.0D+00
  sv3(1) = 0.0D+00
  sv3(2) = 1.0D+00
!
!  User triangle.
!
  tv1(1) = 1.0D+00
  tv1(2) = 0.0D+00
  tv2(1) = 4.0D+00
  tv2(2) = 4.0D+00
  tv3(1) = 0.0D+00
  tv3(2) = 3.0D+00

  seed = 123456789

  do i = 1, 5

    call r8vec_uniform_01 ( 2, seed, sp1 )

    if ( 1.0D+00 < sp1(1) + sp1(2) ) then
      sp1(1) = 1.0D+00 - sp1(1)
      sp1(2) = 1.0D+00 - sp1(2)
    end if

    call simplex_to_triangle ( tv1, tv2, tv3, sp1, tp1 )
    call triangle_to_ref ( tv1, tv2, tv3, tp1, rp1 )
    call ref_to_triangle ( tv1, tv2, tv3, rp1, tp2 )
    call triangle_to_simplex ( tv1, tv2, tv3, tp2, sp2 )

    write ( *, '(a)' ) ''
    write ( *, '(a,2g14.6)' ) '  SP1: ', sp1(1:2)
    write ( *, '(a,2g14.6)' ) '  TP1: ', tp1(1:2)
    write ( *, '(a,2g14.6)' ) '  RP1: ', rp1(1:2)
    write ( *, '(a,2g14.6)' ) '  TP2: ', tp2(1:2)
    write ( *, '(a,2g14.6)' ) '  SP2: ', sp2(1:2)

  end do

  return
end
subroutine test02 ( degree, numnodes, vert1, vert2, vert3 )

!*****************************************************************************80
!
!! TEST02 calls TRIASYMQ for a quadrature rule of given order and region.
!
!  Licensing:
!
!    This code is distributed under the GNU GPL license.
!
!  Modified:
!
!    21 June 2014
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
!    Input, integer ( kind = 4 ) DEGREE, the desired total polynomial degree exactness
!    of the quadrature rule.  0 <= DEGREE <= 50.
!
!    Input, integer ( kind = 4 ) NUMNODES, the number of nodes to be used by the rule.
!
!    Input, real ( kind = 8 ) VERT1(2), VERT2(2), VERT3(2), the
!    vertices of the triangle.
!
  implicit none

  integer ( kind = 4 ) numnodes

  real ( kind = 8 ) area
  real ( kind = 8 ) d
  integer ( kind = 4 ) degree
  integer ( kind = 4 ) j
  real ( kind = 8 ) rnodes(2,numnodes)
  real ( kind = 8 ) triangle_area
  real ( kind = 8 ) vert1(2)
  real ( kind = 8 ) vert2(2)
  real ( kind = 8 ) vert3(2)
  real ( kind = 8 ) weights(numnodes)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  Symmetric quadrature rule for a triangle.'
  write ( *, '(a,i4)' ) &
    '  Polynomial exactness degree DEGREE = ', degree

  area = triangle_area ( vert1, vert2, vert3 )
!
!  Retrieve and print a symmetric quadrature rule.
!
  call triasymq ( degree, vert1, vert2, vert3, rnodes, weights, numnodes )

  write ( *, '(a)' ) ''
  write ( *, '(a,i6)' ) '  NUMNODES = ', numnodes

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) &
    '     J      W               X               Y'
  write ( *, '(a)' ) ''
  do j = 1, numnodes
    write ( *, '(2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      j, weights(j), rnodes(1,j), rnodes(2,j)
  end do

  d = sum ( weights(1:numnodes) )

  write ( *, '(a,2x,g14.6)' ) '   Sum', d
  write ( *, '(a,2x,g14.6)' ) '  Area', area

  return
end
subroutine test03 ( degree, numnodes, vert1, vert2, vert3, header )

!*****************************************************************************80
!
!! TEST03 calls TRIASYMQ_GNUPLOT to generate graphics files.
!
!  Licensing:
!
!    This code is distributed under the GNU GPL license.
!
!  Modified:
!
!    21 June 2014
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
!    Input, integer ( kind = 4 ) DEGREE, the desired total polynomial degree exactness
!    of the quadrature rule.  0 <= DEGREE <= 50.
!
!    Input, integer ( kind = 4 ) NUMNODES, the number of nodes to be used by the rule.
!
!    Input, real ( kind = 8 ) VERT1(2), VERT2(2), VERT3(2), the
!    vertices of the triangle.
!
!    Input, character * ( * ) HEADER, an identifier for the graphics filenames.
!
  implicit none

  integer ( kind = 4 ) numnodes

  integer ( kind = 4 ) degree
  character * ( * ) header
  real ( kind = 8 ) rnodes(2,numnodes)
  real ( kind = 8 ) vert1(2)
  real ( kind = 8 ) vert2(2)
  real ( kind = 8 ) vert3(2)
  real ( kind = 8 ) weights(numnodes)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) &
    '  TRIASYMQ_GNUPLOT creates gnuplot graphics files.'
  write ( *, '(a,i4)' ) &
    '  Polynomial exactness degree DEGREE = ', degree

  call triasymq ( degree, vert1, vert2, vert3, rnodes, weights, &
    numnodes )

  write ( *, '(a,i4)' ) '  Number of nodes = ', numnodes

  call triasymq_gnuplot ( vert1, vert2, vert3, numnodes, rnodes, &
    header )

  return
end
subroutine test04 ( degree, numnodes, vert1, vert2, vert3, header )

!*****************************************************************************80
!
!! TEST04 gets a rule and writes it to a file.
!
!  Licensing:
!
!    This code is distributed under the GNU GPL license.
!
!  Modified:
!
!    20 June 2014
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
!    Input, integer ( kind = 4 ) DEGREE, the desired total polynomial degree exactness
!    of the quadrature rule.  0 <= DEGREE <= 50.
!
!    Input, integer ( kind = 4 ) NUMNODES, the number of nodes to be used by the rule.
!
!    Input, real ( kind = 8 ) VERT1(2), VERT2(2), VERT3(2), the
!    vertices of the triangle.
!
!    Input, character * ( * ) HEADER, an identifier for the filenames.
!
  implicit none

  integer ( kind = 4 ) numnodes

  integer ( kind = 4 ) degree
  character * ( * ) header
  integer ( kind = 4 ) i
  real ( kind = 8 ) rnodes(2,numnodes)
  integer ( kind = 4 ) rule_unit
  character * ( 255 ) rule_filename
  real ( kind = 8 ) vert1(2)
  real ( kind = 8 ) vert2(2)
  real ( kind = 8 ) vert3(2)
  real ( kind = 8 ) weights(numnodes)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST04'
  write ( *, '(a)' ) '  Get a quadrature rule for a triangle.'
  write ( *, '(a)' ) '  Then write it to a file.'
  write ( *, '(a,i4)' ) &
    '  Polynomial exactness degree DEGREE = ', degree
!
!  Retrieve a symmetric quadrature rule.
!
  call triasymq ( degree, vert1, vert2, vert3, rnodes, weights, &
    numnodes )
!
!  Write the points and weights to a file.
!
  call get_unit ( rule_unit )

  rule_filename = trim ( header ) // '.txt'

  open ( unit = rule_unit, file = rule_filename, &
    status = 'replace' )
  do i = 1, numnodes
    write ( rule_unit, '(3(e21.15,2x))' ) &
      rnodes(1,i), rnodes(2,i), weights(i)
  end do
  close ( unit = rule_unit )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Quadrature rule written to file "' &
    // trim ( rule_filename ) // '".'

  return
end
subroutine test05 ( degree, numnodes, vert1, vert2, vert3 )

!*****************************************************************************80
!
!! TEST05 calls TRIASYMQ for a quadrature rule of given order and region.
!
!  Licensing:
!
!    This code is distributed under the GNU GPL license.
!
!  Modified:
!
!    26 June 2014
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
!    Input, integer ( kind = 4 ) DEGREE, the desired total polynomial degree 
!    exactness of the quadrature rule.  0 <= DEGREE <= 50.
!
!    Input, integer ( kind = 4 ) NUMNODES, the number of nodes to be used by the rule.
!
!    Input, real ( kind = 8 ) VERT1(2), VERT2(2), VERT3(2), the
!    vertices of the triangle.
!
  implicit none

  integer ( kind = 4 ) degree
  integer ( kind = 4 ) numnodes

  real ( kind = 8 ) area
  real ( kind = 8 ) d
  integer ( kind = 4 ) i
  integer ( kind = 4 ) npols
  real ( kind = 8 ) pols(((degree+1)*(degree+2))/2)
  real ( kind = 8 ) r(2)
  real ( kind = 8 ) rints(((degree+1)*(degree+2))/2)
  real ( kind = 8 ) rnodes(2,numnodes)
  real ( kind = 8 ) triangle_area
  real ( kind = 8 ) scale
  real ( kind = 8 ) vert1(2)
  real ( kind = 8 ) vert2(2)
  real ( kind = 8 ) vert3(2)
  real ( kind = 8 ) weights(numnodes)
  real ( kind = 8 ) z(2)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST05'
  write ( *, '(a)' ) '  Compute a quadrature rule for a triangle.'
  write ( *, '(a)' ) &
    '  Check it by integrating orthonormal polynomials.'
  write ( *, '(a,i4)' ) '  Polynomial exactness degree DEGREE = ', degree

  area = triangle_area ( vert1, vert2, vert3 )
!
!  Retrieve a symmetric quadrature rule.
!
  call triasymq ( degree, vert1, vert2, vert3, rnodes, weights, numnodes )
!
!  Construct the matrix of values of the orthogonal polynomials
!  at the user-provided nodes
!
  npols = ( degree + 1 ) * ( degree + 2 ) / 2

  rints(1:npols) = 0.0D+00

  do i = 1, numnodes
    z(1) = rnodes(1,i)
    z(2) = rnodes(2,i)
    call triangle_to_ref ( vert1, vert2, vert3, z, r )
    call ortho2eva ( degree, r, pols )
    rints(1:npols) = rints(1:npols) + weights(i) * pols(1:npols)
  end do

  scale = sqrt ( sqrt ( 3.0D+00 ) ) / sqrt ( area )
  rints(1:npols) = rints(1:npols) * scale

  d = ( rints(1) - sqrt ( area ) )**2 + sum ( rints(2:npols)**2 )
  d = sqrt ( d ) / dble ( npols )

  write ( *, '(a)' ) ''
  write ( *, '(a,g14.6)' ) '  RMS integration error = ', d

  return
end
