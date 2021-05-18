program main

!*****************************************************************************80
!
!! MAIN is the main program for SQUARE_ARBQ_RULE_TEST.
!
!  Discussion:
!
!    SQUARE_ARBQ_RULE_TEST tests the SQUARE_ARBQ_RULE library.
!
!  Licensing:
!
!    This code is distributed under the GNU GPL license.
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
  implicit none

  integer ( kind = 4 ) degree
  character * ( 255 ) header
  integer ( kind = 4 ) n

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SQUARE_ARBQ_RULE_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the SQUARE_ARBQ_RULE library.'

  degree = 8
  call square_arbq_size ( degree, n )
  header = 'square08'

  call test01 ( degree, n )

  call test02 ( degree, n, header )

  call test03 ( degree, n, header )

  call test04 ( degree, n )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'SQUARE_ARBQ_RULE_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop 0
end
subroutine test01 ( degree, n )

!*****************************************************************************80
!
!! TEST01 calls SQUARE_ARBQ for a quadrature rule of given order.
!
!  Licensing:
!
!    This code is distributed under the GNU GPL license.
!
!  Modified:
!
!    30 June 2014
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
!    of the quadrature rule.
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) area
  real ( kind = 8 ) d
  integer ( kind = 4 ) degree
  integer ( kind = 4 ) j
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(2,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Quadrature rule for a symmetric square.'
  write ( *, '(a,i4)' )'  Polynomial exactness degree DEGREE = ', degree

  area = 4.0D+00
!
!  Retrieve and print a quadrature rule.
!
  call square_arbq ( degree, n, x, w )

  write ( *, '(a)' ) ''
  write ( *, '(a,i6)' ) '  Number of nodes N = ', n

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '     J  W       X       Y'
  write ( *, '(a)' ) ''
  do j = 1, n
    write ( *, '(2x,i4,2x,g14.6,2x,g14.6,2x,g14.6)' ) &
      j, w(j), x(1,j), x(2,j)
  end do

  d = sum ( w(1:n) )

  write ( *, '(a,2x,g14.6)' ) '   Sum', d
  write ( *, '(a,2x,g14.6)' ) '  Area', area

  return
end
subroutine test02 ( degree, n, header )

!*****************************************************************************80
!
!! TEST02 gets a rule and writes it to a file.
!
!  Licensing:
!
!    This code is distributed under the GNU GPL license.
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
!    Input, integer ( kind = 4 ) DEGREE, the desired total polynomial degree exactness
!    of the quadrature rule.  0 <= DEGREE <= 50.
!
!    Input, integer ( kind = 4 ) N, the number of nodes to be used by the rule.
!
!    Input, character * ( * ) HEADER, an identifier for the filenames.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) degree
  character * ( * ) header
  integer ( kind = 4 ) i
  integer ( kind = 4 ) rule_unit
  character * ( 255 ) rule_filename
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(2,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) '  Get a quadrature rule for the symmetric square.'
  write ( *, '(a)' ) '  Then write it to a file.'
  write ( *, '(a,i4)' ) '  Polynomial exactness degree DEGREE = ', degree
!
!  Retrieve a quadrature rule.
!
  call square_arbq ( degree, n, x, w )
!
!  Write the points and weights to a file.
!
  call get_unit ( rule_unit )

  rule_filename = trim ( header ) // '.txt'

  open ( unit = rule_unit, file = rule_filename, status = 'replace' )
  do i = 1, n
    write ( rule_unit, '(3(e21.15,2x))' ) x(1,i), x(2,i), w(i)
  end do
  close ( unit = rule_unit )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Quadrature rule written to file "' &
    // trim ( rule_filename ) // '".'

  return
end
subroutine test03 ( degree, n, header )

!*****************************************************************************80
!
!! TEST03 gets a rule and creates GNUPLOT input files.
!
!  Licensing:
!
!    This code is distributed under the GNU GPL license.
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
!    Input, integer ( kind = 4 ) DEGREE, the desired total polynomial degree
!    exactness of the quadrature rule.  0 <= DEGREE <= 50.
!
!    Input, integer ( kind = 4 ) N, the number of nodes to be used by the rule.
!
!    Input, character * ( * ) HEADER, an identifier for the filenames.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) degree
  character * ( * ) header
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(2,n)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST03'
  write ( *, '(a)' ) '  Get a quadrature rule for the symmetric square.'
  write ( *, '(a)' ) '  Set up GNUPLOT graphics input.'
  write ( *, '(a,i4)' ) '  Polynomial exactness degree DEGREE = ', degree
!
!  Retrieve a quadrature rule.
!
  call square_arbq ( degree, n, x, w )
!
!  Create files for input to GNUPLOT.
!
  call square_arbq_gnuplot ( n, x, header )

  return
end
subroutine test04 ( degree, n )

!*****************************************************************************80
!
!! TEST04 gets a rule and tests its accuracy.
!
!  Licensing:
!
!    This code is distributed under the GNU GPL license.
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
!    Input, integer ( kind = 4 ) DEGREE, the desired total polynomial degree exactness
!    of the quadrature rule.  0 <= DEGREE <= 50.
!
!    Input, integer ( kind = 4 ) N, the number of nodes to be used by the rule.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ) area
  real ( kind = 8 ) d
  integer ( kind = 4 ) degree
  integer ( kind = 4 ) i
  integer ( kind = 4 ) npols
  real ( kind = 8 ) pols(((degree+1)*(degree+2))/2)
  real ( kind = 8 ) rints(((degree+1)*(degree+2))/2)
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) z(2)

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'TEST04'
  write ( *, '(a)' ) '  Get a quadrature rule for the symmetric square.'
  write ( *, '(a)' ) '  Test its accuracy.'
  write ( *, '(a,i4)' ) '  Polynomial exactness degree DEGREE = ', degree
!
!  Retrieve a quadrature rule.
!
  call square_arbq ( degree, n, x, w )

  npols = ( ( degree + 1 ) * ( degree + 2 ) ) / 2

  rints(1:npols) = 0.0D+00

  do i = 1, n

    z(1) = x(1,i)
    z(2) = x(2,i)

    call lege2eva ( degree, z, pols )

    rints(1:npols) = rints(1:npols) + w(i) * pols(1:npols)
 
  end do

  area = 4.0D+00

  d = 0.0D+00
  d = ( rints(1) - sqrt ( area ) )**2
  do i = 2, npols
    d = d + rints(i)**2
  end do
  d = sqrt ( d ) / real ( npols, kind = 8 )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  RMS error = ', d

  return
end
