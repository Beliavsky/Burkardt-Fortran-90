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
subroutine lege2eva ( degree, z, pols )

!*****************************************************************************80
!
!! LEGE2EVA evaluates orthogonal polynomials on the symmetric square.
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
!    Input, integer ( kind = 4 ) DEGREE, the maximum degree of the polynomials.
!
!    Input, real ( kind = 8 ) Z(2), the evaluation point.
!
!    Output, real ( kind = 8 ) POLS((DEGREE+1)*(DEGREE+2)/2), the orthogonal
!    polynomials evaluated at Z.
!
  implicit none

  integer ( kind = 4 ) degree

  real ( kind = 8 ) f1(degree+1)
  real ( kind = 8 ) f2(degree+1)
  integer ( kind = 4 ) kk
  integer ( kind = 4 ) m
  integer ( kind = 4 ) n
  real ( kind = 8 ) pols(((degree+1)*(degree+2))/2)
  real ( kind = 8 ) scale
  real ( kind = 8 ) z(2)

  call llegepols1 ( degree, z(1), f1 )
  call llegepols1 ( degree, z(2), f2 )

  kk = 0
  do m = 0, degree
    do n = 0, m
      kk = kk + 1
      pols(kk) = f1(m-n+1) * f2(n+1)
      scale = dble ( ( 1 + 2 * n ) * ( 1 + 2 * ( m - n ) ) )
      scale = 0.5D+00 * sqrt ( scale )
      pols(kk) = pols(kk) * scale
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
subroutine rule_full_size ( degree, n )

!*****************************************************************************80
!
!! RULE_FULL_SIZE returns the full size of the requested quadrature rule.
!
!  Licensing:
!
!    This code is distributed under the GNU GPL license.
!
!  Modified:
!
!    24 June 2014
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
!    Input, integer ( kind = 4 ) DEGREE, the degree of the quadrature (the 
!    maximum degree of the polynomials of two variables that are integrated
!    exactly.  1 <= DEGREE <= 20.
!
!    Output, integer ( kind = 4 ) N, the number of nodes in the full rule.
!
  implicit none

  integer ( kind = 4 ) degree
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_save(20)

  save n_save

  data n_save / &
      1,   4,   4,   7,   7,  12,  12,  17,  17,  24, &
     24,  33,  33,  44,  44,  55,  55,  68,  68,  81 /

  if ( 1 <= degree .and. degree <= 20 ) then
    n = n_save(degree)
  else
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'RULE_FULL_SIZE - Fatal error!'
    write ( *, '(a)' ) '  Degree DEGREE must be between 1 and 20.'
    stop 1
  end if

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
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(2,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 1 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
       0.00000000000000000D+00 /
  data ys / &
       0.00000000000000000D+00 /
  data ws / &
       0.28284271247461904D+01/

  x(1,1:n) = xs(1:n)
  x(2,1:n) = ys(1:n)
  w(1:ns) = ws(1:ns)

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
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(2,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 4 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
   -0.5773502691896256D+00, &
    0.5773502691896260D+00, &
    0.5773502691896256D+00, &
   -0.5773502691896260D+00 /
  data ys / &
   -0.5773502691896260D+00, &
   -0.5773502691896256D+00, &
    0.5773502691896260D+00, &
    0.5773502691896256D+00/
  data ws / &
    0.7071067811865476D+00, &
    0.7071067811865476D+00, &
    0.7071067811865476D+00, &
    0.7071067811865476D+00/

  x(1,1:n) = xs(1:n)
  x(2,1:n) = ys(1:n)
  w(1:ns) = ws(1:ns)

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
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(2,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 4 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
   -0.5773502691896256D+00, &
    0.5773502691896260D+00, &
    0.5773502691896256D+00, &
   -0.5773502691896260D+00/
  data ys / &
   -0.5773502691896260D+00, &
   -0.5773502691896256D+00, &
    0.5773502691896260D+00, &
    0.5773502691896256D+00/
  data ws / &
    0.7071067811865476D+00, &
    0.7071067811865476D+00, &
    0.7071067811865476D+00, &
    0.7071067811865476D+00/

  x(1,1:n) = xs(1:n)
  x(2,1:n) = ys(1:n)
  w(1:ns) = ws(1:ns)

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
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(2,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 7 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
    0.3683480503448356D+00, &
   -0.3683480503448355D+00, &
    0.8881837963234579D+00, &
   -0.8881837963234579D+00, &
   -0.6849278434806340D+00, &
    0.6849278434806340D+00, &
    0.1035042199756803D-32 /
  data ys / &
   -0.8931142408116063D+00, &
    0.8931142408116063D+00, &
   -0.3800827242611582D+00, &
    0.3800827242611583D+00, &
   -0.6813275148988932D+00, &
    0.6813275148988932D+00, &
   -0.4874534345070689D-33/
  data ws / &
    0.2922561796990344D+00, &
    0.2922561796990344D+00, &
    0.2970097006317383D+00, &
    0.2970097006317383D+00, &
    0.4208866642214383D+00, &
    0.4208866642214383D+00, &
    0.8081220356417685D+00/

  x(1,1:n) = xs(1:n)
  x(2,1:n) = ys(1:n)
  w(1:ns) = ws(1:ns)

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
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(2,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 7 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
    0.1775868202077551D-01, &
   -0.1775868202077539D-01, &
    0.7788710544649639D+00, &
   -0.7788710544649639D+00, &
   -0.7703781288541645D+00, &
    0.7703781288541645D+00, &
   -0.7490353914168658D-33 /
  data ys / &
   -0.9659285494001192D+00, &
    0.9659285494001192D+00, &
   -0.5715708301251639D+00, &
    0.5715708301251639D+00, &
   -0.5829672991828014D+00, &
    0.5829672991828014D+00, &
    0.1356144833394667D-33/
  data ws / &
    0.2246199725165690D+00, &
    0.2246199725165690D+00, &
    0.3901817339168917D+00, &
    0.3901817339168917D+00, &
    0.3953508381187504D+00, &
    0.3953508381187504D+00, &
    0.8081220356417684D+00/

  x(1,1:n) = xs(1:n)
  x(2,1:n) = ys(1:n)
  w(1:ns) = ws(1:ns)

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
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(2,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 12 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
    0.4595981103653579D-16, &
    0.9258200997725515D+00, &
    0.6742045114073804D-16, &
   -0.9258200997725515D+00, &
   -0.3805544332083157D+00, &
    0.3805544332083157D+00, &
    0.3805544332083157D+00, &
   -0.3805544332083157D+00, &
   -0.8059797829185990D+00, &
    0.8059797829185988D+00, &
    0.8059797829185990D+00, &
   -0.8059797829185988D+00 /
  data ys / &
   -0.9258200997725515D+00, &
   -0.1073032005210112D-16, &
    0.9258200997725515D+00, &
    0.1241105822293750D-15, &
   -0.3805544332083157D+00, &
   -0.3805544332083157D+00, &
    0.3805544332083157D+00, &
    0.3805544332083157D+00, &
   -0.8059797829185988D+00, &
   -0.8059797829185990D+00, &
    0.8059797829185988D+00, &
    0.8059797829185990D+00/
  data ws / &
    0.1711023816204485D+00, &
    0.1711023816204485D+00, &
    0.1711023816204485D+00, &
    0.1711023816204485D+00, &
    0.3681147816131979D+00, &
    0.3681147816131979D+00, &
    0.3681147816131979D+00, &
    0.3681147816131979D+00, &
    0.1678896179529011D+00, &
    0.1678896179529011D+00, &
    0.1678896179529011D+00, &
    0.1678896179529011D+00 /

  x(1,1:n) = xs(1:n)
  x(2,1:n) = ys(1:n)
  w(1:ns) = ws(1:ns)

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
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(2,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 12 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
    0.4595981103653579D-16, &
    0.9258200997725515D+00, &
    0.6742045114073804D-16, &
   -0.9258200997725515D+00, &
   -0.3805544332083157D+00, &
    0.3805544332083157D+00, &
    0.3805544332083157D+00, &
   -0.3805544332083157D+00, &
   -0.8059797829185990D+00, &
    0.8059797829185988D+00, &
    0.8059797829185990D+00, &
   -0.8059797829185988D+00 /
  data ys / &
   -0.9258200997725515D+00, &
   -0.1073032005210112D-16, &
    0.9258200997725515D+00, &
    0.1241105822293750D-15, &
   -0.3805544332083157D+00, &
   -0.3805544332083157D+00, &
    0.3805544332083157D+00, &
    0.3805544332083157D+00, &
   -0.8059797829185988D+00, &
   -0.8059797829185990D+00, &
    0.8059797829185988D+00, &
    0.8059797829185990D+00 /
  data ws / &
    0.1711023816204485D+00, &
    0.1711023816204485D+00, &
    0.1711023816204485D+00, &
    0.1711023816204485D+00, &
    0.3681147816131979D+00, &
    0.3681147816131979D+00, &
    0.3681147816131979D+00, &
    0.3681147816131979D+00, &
    0.1678896179529011D+00, &
    0.1678896179529011D+00, &
    0.1678896179529011D+00, &
    0.1678896179529011D+00 /

  x(1,1:n) = xs(1:n)
  x(2,1:n) = ys(1:n)
  w(1:ns) = ws(1:ns)

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
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(2,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 17 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
    0.6306801197316689D+00, &
    0.9688499663619776D+00, &
   -0.6306801197316687D+00, &
   -0.9688499663619776D+00, &
   -0.7502770999789002D+00, &
    0.9279616459595696D+00, &
    0.7502770999789005D+00, &
   -0.9279616459595696D+00, &
   -0.7620832819261708D-01, &
    0.8526157293336623D+00, &
    0.7620832819261719D-01, &
   -0.8526157293336623D+00, &
   -0.5237358202144292D+00, &
    0.4533398211356472D+00, &
    0.5237358202144292D+00, &
   -0.4533398211356471D+00, &
    0.1018964154952896D-32 /
  data ys / &
   -0.9688499663619776D+00, &
    0.6306801197316688D+00, &
    0.9688499663619776D+00, &
   -0.6306801197316686D+00, &
   -0.9279616459595696D+00, &
   -0.7502770999789004D+00, &
    0.9279616459595696D+00, &
    0.7502770999789006D+00, &
   -0.8526157293336623D+00, &
   -0.7620832819261714D-01, &
    0.8526157293336623D+00, &
    0.7620832819261725D-01, &
   -0.4533398211356472D+00, &
   -0.5237358202144292D+00, &
    0.4533398211356471D+00, &
    0.5237358202144292D+00, &
   -0.7403196379681869D-32 /
  data ws / &
    0.6284721101179121D-01, &
    0.6284721101179121D-01, &
    0.6284721101179121D-01, &
    0.6284721101179121D-01, &
    0.7926638883415160D-01, &
    0.7926638883415160D-01, &
    0.7926638883415160D-01, &
    0.7926638883415160D-01, &
    0.1902480253324004D+00, &
    0.1902480253324004D+00, &
    0.1902480253324004D+00, &
    0.1902480253324004D+00, &
    0.2816282136297291D+00, &
    0.2816282136297291D+00, &
    0.2816282136297291D+00, &
    0.2816282136297291D+00, &
    0.3724677695139019D+00 /

  x(1,1:n) = xs(1:n)
  x(2,1:n) = ys(1:n)
  w(1:ns) = ws(1:ns)

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
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(2,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 17 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
    0.6306801197316689D+00, &
    0.9688499663619776D+00, &
   -0.6306801197316687D+00, &
   -0.9688499663619776D+00, &
   -0.7502770999789002D+00, &
    0.9279616459595696D+00, &
    0.7502770999789005D+00, &
   -0.9279616459595696D+00, &
   -0.7620832819261708D-01, &
    0.8526157293336623D+00, &
    0.7620832819261719D-01, &
   -0.8526157293336623D+00, &
   -0.5237358202144292D+00, &
    0.4533398211356472D+00, &
    0.5237358202144292D+00, &
   -0.4533398211356471D+00, &
    0.1018964154952896D-32 /
  data ys / &
   -0.9688499663619776D+00, &
    0.6306801197316688D+00, &
    0.9688499663619776D+00, &
   -0.6306801197316686D+00, &
   -0.9279616459595696D+00, &
   -0.7502770999789004D+00, &
    0.9279616459595696D+00, &
    0.7502770999789006D+00, &
   -0.8526157293336623D+00, &
   -0.7620832819261714D-01, &
    0.8526157293336623D+00, &
    0.7620832819261725D-01, &
   -0.4533398211356472D+00, &
   -0.5237358202144292D+00, &
    0.4533398211356471D+00, &
    0.5237358202144292D+00, &
   -0.7403196379681869D-32 /
  data ws / &
    0.6284721101179121D-01, &
    0.6284721101179121D-01, &
    0.6284721101179121D-01, &
    0.6284721101179121D-01, &
    0.7926638883415160D-01, &
    0.7926638883415160D-01, &
    0.7926638883415160D-01, &
    0.7926638883415160D-01, &
    0.1902480253324004D+00, &
    0.1902480253324004D+00, &
    0.1902480253324004D+00, &
    0.1902480253324004D+00, &
    0.2816282136297291D+00, &
    0.2816282136297291D+00, &
    0.2816282136297291D+00, &
    0.2816282136297291D+00, &
    0.3724677695139019D+00 /

  x(1,1:n) = xs(1:n)
  x(2,1:n) = ys(1:n)
  w(1:ns) = ws(1:ns)

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
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(2,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 24 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
   -0.6980761045495689D+00, &
    0.9826392235408554D+00, &
    0.6980761045495691D+00, &
   -0.9826392235408554D+00, &
    0.8257758359029634D+00, &
    0.9394863828167371D+00, &
   -0.8257758359029632D+00, &
   -0.9394863828167371D+00, &
    0.1885861387186400D+00, &
    0.9535395282015321D+00, &
   -0.1885861387186399D+00, &
   -0.9535395282015321D+00, &
   -0.7120019130753369D+00, &
    0.5253202503645465D+00, &
    0.7120019130753369D+00, &
   -0.5253202503645465D+00, &
   -0.3156234329152560D+00, &
    0.8125205483048131D+00, &
    0.3156234329152561D+00, &
   -0.8125205483048131D+00, &
   -0.4248472488486695D+00, &
    0.4165807191202114D-01, &
    0.4248472488486695D+00, &
   -0.4165807191202109D-01 /
  data ys / &
   -0.9826392235408554D+00, &
   -0.6980761045495690D+00, &
    0.9826392235408554D+00, &
    0.6980761045495693D+00, &
   -0.9394863828167371D+00, &
    0.8257758359029633D+00, &
    0.9394863828167371D+00, &
   -0.8257758359029631D+00, &
   -0.9535395282015321D+00, &
    0.1885861387186400D+00, &
    0.9535395282015321D+00, &
   -0.1885861387186399D+00, &
   -0.5253202503645465D+00, &
   -0.7120019130753369D+00, &
    0.5253202503645465D+00, &
    0.7120019130753369D+00, &
   -0.8125205483048131D+00, &
   -0.3156234329152560D+00, &
    0.8125205483048131D+00, &
    0.3156234329152561D+00, &
   -0.4165807191202117D-01, &
   -0.4248472488486695D+00, &
    0.4165807191202112D-01, &
    0.4248472488486695D+00 /
  data ws / &
    0.3395580740305119D-01, &
    0.3395580740305119D-01, &
    0.3395580740305119D-01, &
    0.3395580740305119D-01, &
    0.4671948489426219D-01, &
    0.4671948489426219D-01, &
    0.4671948489426219D-01, &
    0.4671948489426219D-01, &
    0.6886285066821875D-01, &
    0.6886285066821875D-01, &
    0.6886285066821875D-01, &
    0.6886285066821875D-01, &
    0.1595417182608940D+00, &
    0.1595417182608940D+00, &
    0.1595417182608940D+00, &
    0.1595417182608940D+00, &
    0.1497202089079447D+00, &
    0.1497202089079447D+00, &
    0.1497202089079447D+00, &
    0.1497202089079447D+00, &
    0.2483067110521768D+00, &
    0.2483067110521768D+00, &
    0.2483067110521768D+00, &
    0.2483067110521768D+00 /

  x(1,1:n) = xs(1:n)
  x(2,1:n) = ys(1:n)
  w(1:ns) = ws(1:ns)

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
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(2,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 24 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
    0.1885861387186414D+00, &
    0.9535395282015320D+00, &
   -0.1885861387186413D+00, &
   -0.9535395282015320D+00, &
   -0.6980761045495679D+00, &
    0.9826392235408555D+00, &
    0.6980761045495681D+00, &
   -0.9826392235408555D+00, &
   -0.9394863828167370D+00, &
    0.8257758359029639D+00, &
    0.9394863828167370D+00, &
   -0.8257758359029637D+00, &
   -0.7120019130753364D+00, &
    0.5253202503645475D+00, &
    0.7120019130753364D+00, &
   -0.5253202503645475D+00, &
   -0.3156234329152547D+00, &
    0.8125205483048131D+00, &
    0.3156234329152548D+00, &
   -0.8125205483048131D+00, &
   -0.4248472488486694D+00, &
    0.4165807191202203D-01, &
    0.4248472488486694D+00, &
   -0.4165807191202197D-01 /
  data ys / &
   -0.9535395282015320D+00, &
    0.1885861387186414D+00, &
    0.9535395282015320D+00, &
   -0.1885861387186413D+00, &
   -0.9826392235408555D+00, &
   -0.6980761045495680D+00, &
    0.9826392235408555D+00, &
    0.6980761045495683D+00, &
   -0.8257758359029640D+00, &
   -0.9394863828167370D+00, &
    0.8257758359029638D+00, &
    0.9394863828167370D+00, &
   -0.5253202503645475D+00, &
   -0.7120019130753364D+00, &
    0.5253202503645475D+00, &
    0.7120019130753364D+00, &
   -0.8125205483048131D+00, &
   -0.3156234329152547D+00, &
    0.8125205483048131D+00, &
    0.3156234329152549D+00, &
   -0.4165807191202205D-01, &
   -0.4248472488486694D+00, &
    0.4165807191202200D-01, &
    0.4248472488486694D+00 /
  data ws / &
    0.6886285066821880D-01, &
    0.6886285066821880D-01, &
    0.6886285066821880D-01, &
    0.6886285066821880D-01, &
    0.3395580740305121D-01, &
    0.3395580740305121D-01, &
    0.3395580740305121D-01, &
    0.3395580740305121D-01, &
    0.4671948489426224D-01, &
    0.4671948489426224D-01, &
    0.4671948489426224D-01, &
    0.4671948489426224D-01, &
    0.1595417182608939D+00, &
    0.1595417182608939D+00, &
    0.1595417182608939D+00, &
    0.1595417182608939D+00, &
    0.1497202089079448D+00, &
    0.1497202089079448D+00, &
    0.1497202089079448D+00, &
    0.1497202089079448D+00, &
    0.2483067110521767D+00, &
    0.2483067110521767D+00, &
    0.2483067110521767D+00, &
    0.2483067110521767D+00 /

  x(1,1:n) = xs(1:n)
  x(2,1:n) = ys(1:n)
  w(1:ns) = ws(1:ns)

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
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(2,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 33 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
   -0.9572976997863073D+00, &
    0.8595560056416388D+00, &
    0.9572976997863073D+00, &
   -0.8595560056416386D+00, &
   -0.7788097115544194D+00, &
    0.9834866824398721D+00, &
    0.7788097115544196D+00, &
   -0.9834866824398721D+00, &
   -0.4758086252182758D+00, &
    0.8500766736997486D+00, &
    0.4758086252182759D+00, &
   -0.8500766736997486D+00, &
   -0.7558053565720815D+00, &
    0.6478216371870107D+00, &
    0.7558053565720815D+00, &
   -0.6478216371870107D+00, &
   -0.3427165560404068D+00, &
    0.4093045616940387D+00, &
    0.3427165560404068D+00, &
   -0.4093045616940387D+00, &
   -0.1381834598624653D+00, &
    0.9589251702875349D+00, &
    0.1381834598624654D+00, &
   -0.9589251702875349D+00, &
    0.7074150899644485D-01, &
    0.6962500784917494D+00, &
   -0.7074150899644477D-01, &
   -0.6962500784917494D+00, &
    0.3907362161294610D+00, &
    0.9413272258729252D+00, &
   -0.3907362161294609D+00, &
   -0.9413272258729252D+00, &
   -0.3126032252245169D-31 /
  data ys / &
   -0.8595560056416389D+00, &
   -0.9572976997863073D+00, &
    0.8595560056416387D+00, &
    0.9572976997863073D+00, &
   -0.9834866824398721D+00, &
   -0.7788097115544195D+00, &
    0.9834866824398721D+00, &
    0.7788097115544197D+00, &
   -0.8500766736997486D+00, &
   -0.4758086252182758D+00, &
    0.8500766736997486D+00, &
    0.4758086252182759D+00, &
   -0.6478216371870107D+00, &
   -0.7558053565720815D+00, &
    0.6478216371870107D+00, &
    0.7558053565720815D+00, &
   -0.4093045616940387D+00, &
   -0.3427165560404068D+00, &
    0.4093045616940387D+00, &
    0.3427165560404068D+00, &
   -0.9589251702875349D+00, &
   -0.1381834598624653D+00, &
    0.9589251702875349D+00, &
    0.1381834598624654D+00, &
   -0.6962500784917494D+00, &
    0.7074150899644481D-01, &
    0.6962500784917494D+00, &
   -0.7074150899644473D-01, &
   -0.9413272258729252D+00, &
    0.3907362161294610D+00, &
    0.9413272258729252D+00, &
   -0.3907362161294609D+00, &
   -0.1114446878059780D-31 /
  data ws / &
    0.2699339218118220D-01, &
    0.2699339218118220D-01, &
    0.2699339218118220D-01, &
    0.2699339218118220D-01, &
    0.2120743264134161D-01, &
    0.2120743264134161D-01, &
    0.2120743264134161D-01, &
    0.2120743264134161D-01, &
    0.8403587015611028D-01, &
    0.8403587015611028D-01, &
    0.8403587015611028D-01, &
    0.8403587015611028D-01, &
    0.9175668641747105D-01, &
    0.9175668641747105D-01, &
    0.9175668641747105D-01, &
    0.9175668641747105D-01, &
    0.1816350488471703D+00, &
    0.1816350488471703D+00, &
    0.1816350488471703D+00, &
    0.1816350488471703D+00, &
    0.4272687338421145D-01, &
    0.4272687338421145D-01, &
    0.4272687338421145D-01, &
    0.4272687338421145D-01, &
    0.1508552789574408D+00, &
    0.1508552789574408D+00, &
    0.1508552789574408D+00, &
    0.1508552789574408D+00, &
    0.5479564090947486D-01, &
    0.5479564090947486D-01, &
    0.5479564090947486D-01, &
    0.5479564090947486D-01, &
    0.2124022307685798D+00 /

  x(1,1:n) = xs(1:n)
  x(2,1:n) = ys(1:n)
  w(1:ns) = ws(1:ns)

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
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(2,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 33 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
   -0.9572976997863074D+00, &
    0.8595560056416388D+00, &
    0.9572976997863074D+00, &
   -0.8595560056416386D+00, &
   -0.7788097115544195D+00, &
    0.9834866824398722D+00, &
    0.7788097115544197D+00, &
   -0.9834866824398722D+00, &
   -0.4758086252182752D+00, &
    0.8500766736997490D+00, &
    0.4758086252182753D+00, &
   -0.8500766736997490D+00, &
    0.3907362161294613D+00, &
    0.9413272258729251D+00, &
   -0.3907362161294612D+00, &
   -0.9413272258729251D+00, &
   -0.1381834598624646D+00, &
    0.9589251702875351D+00, &
    0.1381834598624647D+00, &
   -0.9589251702875351D+00, &
    0.6478216371870111D+00, &
    0.7558053565720809D+00, &
   -0.6478216371870111D+00, &
   -0.7558053565720809D+00, &
    0.7074150899644462D-01, &
    0.6962500784917495D+00, &
   -0.7074150899644453D-01, &
   -0.6962500784917495D+00, &
   -0.3427165560404070D+00, &
    0.4093045616940387D+00, &
    0.3427165560404070D+00, &
   -0.4093045616940387D+00, &
   -0.7375869198366919D-30 /
  data ys / &
   -0.8595560056416389D+00, &
   -0.9572976997863074D+00, &
    0.8595560056416387D+00, &
    0.9572976997863074D+00, &
   -0.9834866824398722D+00, &
   -0.7788097115544196D+00, &
    0.9834866824398722D+00, &
    0.7788097115544198D+00, &
   -0.8500766736997490D+00, &
   -0.4758086252182752D+00, &
    0.8500766736997490D+00, &
    0.4758086252182753D+00, &
   -0.9413272258729251D+00, &
    0.3907362161294612D+00, &
    0.9413272258729251D+00, &
   -0.3907362161294611D+00, &
   -0.9589251702875351D+00, &
   -0.1381834598624647D+00, &
    0.9589251702875351D+00, &
    0.1381834598624648D+00, &
   -0.7558053565720809D+00, &
    0.6478216371870111D+00, &
    0.7558053565720809D+00, &
   -0.6478216371870111D+00, &
   -0.6962500784917495D+00, &
    0.7074150899644457D-01, &
    0.6962500784917495D+00, &
   -0.7074150899644449D-01, &
   -0.4093045616940387D+00, &
   -0.3427165560404070D+00, &
    0.4093045616940387D+00, &
    0.3427165560404070D+00, &
   -0.6522588594679827D-30 /
  data ws / &
    0.2699339218118215D-01, &
    0.2699339218118215D-01, &
    0.2699339218118215D-01, &
    0.2699339218118215D-01, &
    0.2120743264134157D-01, &
    0.2120743264134157D-01, &
    0.2120743264134157D-01, &
    0.2120743264134157D-01, &
    0.8403587015611026D-01, &
    0.8403587015611026D-01, &
    0.8403587015611026D-01, &
    0.8403587015611026D-01, &
    0.5479564090947502D-01, &
    0.5479564090947502D-01, &
    0.5479564090947502D-01, &
    0.5479564090947502D-01, &
    0.4272687338421139D-01, &
    0.4272687338421139D-01, &
    0.4272687338421139D-01, &
    0.4272687338421139D-01, &
    0.9175668641747110D-01, &
    0.9175668641747110D-01, &
    0.9175668641747110D-01, &
    0.9175668641747110D-01, &
    0.1508552789574409D+00, &
    0.1508552789574409D+00, &
    0.1508552789574409D+00, &
    0.1508552789574409D+00, &
    0.1816350488471704D+00, &
    0.1816350488471704D+00, &
    0.1816350488471704D+00, &
    0.1816350488471704D+00, &
    0.2124022307685795D+00 /

  x(1,1:n) = xs(1:n)
  x(2,1:n) = ys(1:n)
  w(1:ns) = ws(1:ns)

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
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(2,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 44 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
   -0.6714783701550190D+00, &
    0.9859876542016408D+00, &
    0.6714783701550192D+00, &
   -0.9859876542016408D+00, &
   -0.9318844245957986D+00, &
    0.9382770335701854D+00, &
    0.9318844245957988D+00, &
   -0.9382770335701852D+00, &
    0.6776977793098985D+00, &
    0.9773357693271729D+00, &
   -0.6776977793098983D+00, &
   -0.9773357693271729D+00, &
    0.4073679548284153D+00, &
    0.8648066658739809D+00, &
   -0.4073679548284151D+00, &
   -0.8648066658739809D+00, &
    0.6518175069036650D-01, &
    0.9759935658724420D+00, &
   -0.6518175069036639D-01, &
   -0.9759935658724420D+00, &
   -0.7473119631960774D+00, &
    0.7834652444128232D+00, &
    0.7473119631960774D+00, &
   -0.7834652444128232D+00, &
    0.1328305205898269D+00, &
    0.6241210323620054D+00, &
   -0.1328305205898269D+00, &
   -0.6241210323620054D+00, &
   -0.4781379108769722D+00, &
    0.5501448214169192D+00, &
    0.4781379108769723D+00, &
   -0.5501448214169192D+00, &
   -0.1803286643164523D+00, &
    0.8053335984690123D+00, &
    0.1803286643164524D+00, &
   -0.8053335984690123D+00, &
   -0.4134760830488010D+00, &
    0.9261965849285028D+00, &
    0.4134760830488011D+00, &
   -0.9261965849285028D+00, &
   -0.1307639250027494D+00, &
    0.2910908755606336D+00, &
    0.1307639250027494D+00, &
   -0.2910908755606336D+00 /
  data ys / &
   -0.9859876542016408D+00, &
   -0.6714783701550191D+00, &
    0.9859876542016408D+00, &
    0.6714783701550193D+00, &
   -0.9382770335701855D+00, &
   -0.9318844245957987D+00, &
    0.9382770335701853D+00, &
    0.9318844245957989D+00, &
   -0.9773357693271729D+00, &
    0.6776977793098984D+00, &
    0.9773357693271729D+00, &
   -0.6776977793098982D+00, &
   -0.8648066658739809D+00, &
    0.4073679548284152D+00, &
    0.8648066658739809D+00, &
   -0.4073679548284151D+00, &
   -0.9759935658724420D+00, &
    0.6518175069036644D-01, &
    0.9759935658724420D+00, &
   -0.6518175069036633D-01, &
   -0.7834652444128232D+00, &
   -0.7473119631960774D+00, &
    0.7834652444128232D+00, &
    0.7473119631960774D+00, &
   -0.6241210323620054D+00, &
    0.1328305205898269D+00, &
    0.6241210323620054D+00, &
   -0.1328305205898269D+00, &
   -0.5501448214169192D+00, &
   -0.4781379108769723D+00, &
    0.5501448214169192D+00, &
    0.4781379108769724D+00, &
   -0.8053335984690123D+00, &
   -0.1803286643164524D+00, &
    0.8053335984690123D+00, &
    0.1803286643164525D+00, &
   -0.9261965849285028D+00, &
   -0.4134760830488011D+00, &
    0.9261965849285028D+00, &
    0.4134760830488012D+00, &
   -0.2910908755606336D+00, &
   -0.1307639250027494D+00, &
    0.2910908755606336D+00, &
    0.1307639250027494D+00 /
  data ws / &
    0.1410384661573933D-01, &
    0.1410384661573933D-01, &
    0.1410384661573933D-01, &
    0.1410384661573933D-01, &
    0.1896652423210582D-01, &
    0.1896652423210582D-01, &
    0.1896652423210582D-01, &
    0.1896652423210582D-01, &
    0.2088141025507279D-01, &
    0.2088141025507279D-01, &
    0.2088141025507279D-01, &
    0.2088141025507279D-01, &
    0.7331394692154988D-01, &
    0.7331394692154988D-01, &
    0.7331394692154988D-01, &
    0.7331394692154988D-01, &
    0.3078002143226069D-01, &
    0.3078002143226069D-01, &
    0.3078002143226069D-01, &
    0.3078002143226069D-01, &
    0.6693059666394105D-01, &
    0.6693059666394105D-01, &
    0.6693059666394105D-01, &
    0.6693059666394105D-01, &
    0.1122840307920054D+00, &
    0.1122840307920054D+00, &
    0.1122840307920054D+00, &
    0.1122840307920054D+00, &
    0.1159261595200915D+00, &
    0.1159261595200915D+00, &
    0.1159261595200915D+00, &
    0.1159261595200915D+00, &
    0.7346051498025349D-01, &
    0.7346051498025349D-01, &
    0.7346051498025349D-01, &
    0.7346051498025349D-01, &
    0.4099703937729331D-01, &
    0.4099703937729331D-01, &
    0.4099703937729331D-01, &
    0.4099703937729331D-01, &
    0.1394626903962344D+00, &
    0.1394626903962344D+00, &
    0.1394626903962344D+00, &
    0.1394626903962344D+00 /

  x(1,1:n) = xs(1:n)
  x(2,1:n) = ys(1:n)
  w(1:ns) = ws(1:ns)

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
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(2,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 44 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
    0.7749527857778351D+00, &
    0.9885448991378063D+00, &
   -0.7749527857778349D+00, &
   -0.9885448991378063D+00, &
   -0.9070374303651182D+00, &
    0.9571446613308432D+00, &
    0.9070374303651184D+00, &
   -0.9571446613308430D+00, &
   -0.4303978306869286D+00, &
    0.9769578054468787D+00, &
    0.4303978306869287D+00, &
   -0.9769578054468787D+00, &
   -0.9756646723906326D+00, &
    0.1107064048513496D+00, &
    0.9756646723906326D+00, &
   -0.1107064048513495D+00, &
   -0.7388921437312957D+00, &
    0.7868610204187212D+00, &
    0.7388921437312957D+00, &
   -0.7868610204187212D+00, &
    0.1995220876718269D+00, &
    0.6659287668239546D+00, &
   -0.1995220876718268D+00, &
   -0.6659287668239546D+00, &
   -0.1934983412061240D+00, &
    0.8412271039808018D+00, &
    0.1934983412061241D+00, &
   -0.8412271039808018D+00, &
    0.4882189227791580D+00, &
    0.8922368778153702D+00, &
   -0.4882189227791579D+00, &
   -0.8922368778153702D+00, &
   -0.5772265461040059D+00, &
    0.9526539504944950D+00, &
    0.5772265461040061D+00, &
   -0.9526539504944950D+00, &
   -0.4474426063114782D+00, &
    0.5675455860909890D+00, &
    0.4474426063114783D+00, &
   -0.5675455860909890D+00, &
   -0.7044956995149931D-01, &
    0.3256679896817100D+00, &
    0.7044956995149934D-01, &
   -0.3256679896817100D+00 /
  data ys / &
   -0.9885448991378063D+00, &
    0.7749527857778350D+00, &
    0.9885448991378063D+00, &
   -0.7749527857778348D+00, &
   -0.9571446613308433D+00, &
   -0.9070374303651183D+00, &
    0.9571446613308431D+00, &
    0.9070374303651185D+00, &
   -0.9769578054468787D+00, &
   -0.4303978306869286D+00, &
    0.9769578054468787D+00, &
    0.4303978306869287D+00, &
   -0.1107064048513496D+00, &
   -0.9756646723906326D+00, &
    0.1107064048513495D+00, &
    0.9756646723906326D+00, &
   -0.7868610204187212D+00, &
   -0.7388921437312957D+00, &
    0.7868610204187212D+00, &
    0.7388921437312957D+00, &
   -0.6659287668239546D+00, &
    0.1995220876718268D+00, &
    0.6659287668239546D+00, &
   -0.1995220876718268D+00, &
   -0.8412271039808018D+00, &
   -0.1934983412061240D+00, &
    0.8412271039808018D+00, &
    0.1934983412061241D+00, &
   -0.8922368778153702D+00, &
    0.4882189227791580D+00, &
    0.8922368778153702D+00, &
   -0.4882189227791578D+00, &
   -0.9526539504944950D+00, &
   -0.5772265461040060D+00, &
    0.9526539504944950D+00, &
    0.5772265461040063D+00, &
   -0.5675455860909890D+00, &
   -0.4474426063114783D+00, &
    0.5675455860909890D+00, &
    0.4474426063114784D+00, &
   -0.3256679896817100D+00, &
   -0.7044956995149933D-01, &
    0.3256679896817100D+00, &
    0.7044956995149936D-01 /
  data ws / &
    0.1443015463807196D-01, &
    0.1443015463807196D-01, &
    0.1443015463807196D-01, &
    0.1443015463807196D-01, &
    0.1816242330920956D-01, &
    0.1816242330920956D-01, &
    0.1816242330920956D-01, &
    0.1816242330920956D-01, &
    0.1290815898308381D-01, &
    0.1290815898308381D-01, &
    0.1290815898308381D-01, &
    0.1290815898308381D-01, &
    0.3010764365372140D-01, &
    0.3010764365372140D-01, &
    0.3010764365372140D-01, &
    0.3010764365372140D-01, &
    0.6540469907131932D-01, &
    0.6540469907131932D-01, &
    0.6540469907131932D-01, &
    0.6540469907131932D-01, &
    0.1197895531736646D+00, &
    0.1197895531736646D+00, &
    0.1197895531736646D+00, &
    0.1197895531736646D+00, &
    0.8473841548096289D-01, &
    0.8473841548096289D-01, &
    0.8473841548096289D-01, &
    0.8473841548096289D-01, &
    0.6453833756714425D-01, &
    0.6453833756714425D-01, &
    0.6453833756714425D-01, &
    0.6453833756714425D-01, &
    0.2403055376316494D-01, &
    0.2403055376316494D-01, &
    0.2403055376316494D-01, &
    0.2403055376316494D-01, &
    0.1196130510491228D+00, &
    0.1196130510491228D+00, &
    0.1196130510491228D+00, &
    0.1196130510491228D+00, &
    0.1533837904970821D+00, &
    0.1533837904970821D+00, &
    0.1533837904970821D+00, &
    0.1533837904970821D+00 /

  x(1,1:n) = xs(1:n)
  x(2,1:n) = ys(1:n)
  w(1:ns) = ws(1:ns)

  return
end
subroutine rule16 ( n, x, w )

!*****************************************************************************80
!
!! RULE16 returns the rule of degree 16.
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
!    Output, real ( kind = 8 ) X(2,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 55 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
    0.7331873192446229D+00, &
   -0.7331873192446227D+00, &
   -0.9811278880414770D+00, &
    0.9811278880414772D+00, &
   -0.8004995596996590D+00, &
    0.8004995596996592D+00, &
    0.2935594202060772D+00, &
   -0.2935594202060772D+00, &
    0.5019013651861420D+00, &
   -0.5019013651861418D+00, &
   -0.9240427888147712D+00, &
    0.9240427888147712D+00, &
   -0.7321159842417640D+00, &
    0.7321159842417640D+00, &
    0.9107218705094187D+00, &
   -0.9107218705094184D+00, &
    0.9799531606782582D+00, &
   -0.9799531606782582D+00, &
   -0.2536359436096021D+00, &
    0.2536359436096021D+00, &
    0.8800049697526030D+00, &
   -0.8800049697526030D+00, &
    0.7136219272623606D+00, &
   -0.7136219272623606D+00, &
    0.5185051092186185D+00, &
   -0.5185051092186185D+00, &
    0.9890262305049052D+00, &
   -0.9890262305049052D+00, &
    0.9865971248382277D+00, &
   -0.9865971248382277D+00, &
    0.4087785918187709D-01, &
   -0.4087785918187702D-01, &
    0.9650604144351506D+00, &
   -0.9650604144351506D+00, &
   -0.5228670170578392D+00, &
    0.5228670170578394D+00, &
   -0.2304316370092423D+00, &
    0.2304316370092424D+00, &
    0.7381821882135022D+00, &
   -0.7381821882135022D+00, &
   -0.4979206093242921D+00, &
    0.4979206093242922D+00, &
    0.8494669121845019D+00, &
   -0.8494669121845019D+00, &
    0.4390176422841324D+00, &
   -0.4390176422841323D+00, &
    0.1590601194183188D+00, &
   -0.1590601194183187D+00, &
    0.8973818517920210D+00, &
   -0.8973818517920210D+00, &
    0.6726312443333152D+00, &
   -0.6726312443333152D+00, &
   -0.1686064273871127D+00, &
    0.1686064273871128D+00, &
   -0.3548241530243386D-18 /
  data ys / &
   -0.9711078221435576D+00, &
    0.9711078221435576D+00, &
   -0.9668551959097115D+00, &
    0.9668551959097113D+00, &
   -0.9746926011666336D+00, &
    0.9746926011666336D+00, &
   -0.3231309208576288D+00, &
    0.3231309208576288D+00, &
   -0.9765444785368099D+00, &
    0.9765444785368099D+00, &
   -0.8490306235166675D+00, &
    0.8490306235166672D+00, &
   -0.7537198042004623D+00, &
    0.7537198042004623D+00, &
   -0.9737587969123404D+00, &
    0.9737587969123406D+00, &
   -0.3822148312292263D+00, &
    0.3822148312292264D+00, &
   -0.2988363050086515D+00, &
    0.2988363050086515D+00, &
    0.4849608774128832D+00, &
   -0.4849608774128831D+00, &
    0.2492237020321146D+00, &
   -0.2492237020321144D+00, &
   -0.3504141436316342D-01, &
    0.3504141436316349D-01, &
    0.6278936489285102D+00, &
   -0.6278936489285100D+00, &
   -0.8591476119499135D+00, &
    0.8591476119499137D+00, &
   -0.5892598635566724D+00, &
    0.5892598635566724D+00, &
    0.1438346146728415D+00, &
   -0.1438346146728414D+00, &
   -0.9289486752701194D+00, &
    0.9289486752701194D+00, &
   -0.8028060773786958D+00, &
    0.8028060773786958D+00, &
   -0.8651144139342870D+00, &
    0.8651144139342870D+00, &
   -0.5653829126627348D+00, &
    0.5653829126627348D+00, &
   -0.1574661586091270D+00, &
    0.1574661586091272D+00, &
   -0.7312745784466166D+00, &
    0.7312745784466166D+00, &
   -0.9115177107109407D+00, &
    0.9115177107109407D+00, &
   -0.6626783799774586D+00, &
    0.6626783799774586D+00, &
   -0.4696061222418765D+00, &
    0.4696061222418766D+00, &
   -0.9939228673343959D+00, &
    0.9939228673343959D+00, &
    0.3228625474392587D-19 /
  data ws / &
    0.3224472434909546D-02, &
    0.3224472434909546D-02, &
    0.4080157527921578D-02, &
    0.4080157527921578D-02, &
    0.1406321867924724D-01, &
    0.1406321867924724D-01, &
    0.1094478053582958D+00, &
    0.1094478053582958D+00, &
    0.2046021623250057D-01, &
    0.2046021623250057D-01, &
    0.2244481290183435D-01, &
    0.2244481290183435D-01, &
    0.5310357585578484D-01, &
    0.5310357585578484D-01, &
    0.1049750419840204D-01, &
    0.1049750419840204D-01, &
    0.2100735514277743D-01, &
    0.2100735514277743D-01, &
    0.1140510361065565D+00, &
    0.1140510361065565D+00, &
    0.4811709451294231D-01, &
    0.4811709451294231D-01, &
    0.7994419804097108D-01, &
    0.7994419804097108D-01, &
    0.1010005451633049D+00, &
    0.1010005451633049D+00, &
    0.1204195881877324D-01, &
    0.1204195881877324D-01, &
    0.9474459024829298D-02, &
    0.9474459024829298D-02, &
    0.1005514424347678D+00, &
    0.1005514424347678D+00, &
    0.3161642787539286D-01, &
    0.3161642787539286D-01, &
    0.3963833050663004D-01, &
    0.3963833050663004D-01, &
    0.7350586661049985D-01, &
    0.7350586661049985D-01, &
    0.4319417861510279D-01, &
    0.4319417861510279D-01, &
    0.8810251098693814D-01, &
    0.8810251098693814D-01, &
    0.6864316028539075D-01, &
    0.6864316028539075D-01, &
    0.8257746135731812D-01, &
    0.8257746135731812D-01, &
    0.5439632620644287D-01, &
    0.5439632620644287D-01, &
    0.4386704732153978D-01, &
    0.4386704732153978D-01, &
    0.8808225769982879D-01, &
    0.8808225769982879D-01, &
    0.1534893259270625D-01, &
    0.1534893259270625D-01, &
    0.1234624197629746D+00 /

  x(1,1:n) = xs(1:n)
  x(2,1:n) = ys(1:n)
  w(1:ns) = ws(1:ns)

  return
end
subroutine rule17 ( n, x, w )

!*****************************************************************************80
!
!! RULE17 returns the rule of degree 17.
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
!    Output, real ( kind = 8 ) X(2,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 55 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
   -0.7710386602263628D+00, &
    0.7710386602263630D+00, &
    0.9803457456469387D+00, &
   -0.9803457456469384D+00, &
   -0.2292639639675523D+00, &
    0.2292639639675524D+00, &
    0.4847176019505991D-03, &
   -0.4847176019504780D-03, &
   -0.6189416389750175D+00, &
    0.6189416389750177D+00, &
    0.9587315519802511D+00, &
   -0.9587315519802511D+00, &
    0.8409306922533750D+00, &
   -0.8409306922533748D+00, &
   -0.4308042054877432D+00, &
    0.4308042054877433D+00, &
    0.4761431266211590D+00, &
   -0.4761431266211589D+00, &
    0.8651144531733139D+00, &
   -0.8651144531733139D+00, &
    0.9846617345267017D+00, &
   -0.9846617345267017D+00, &
   -0.7981639404863030D+00, &
    0.7981639404863030D+00, &
    0.6877591943414725D+00, &
   -0.6877591943414725D+00, &
   -0.3038305486106544D+00, &
    0.3038305486106544D+00, &
    0.9852576255116258D+00, &
   -0.9852576255116258D+00, &
    0.9853756930046446D+00, &
   -0.9853756930046446D+00, &
    0.7024672194580522D+00, &
   -0.7024672194580522D+00, &
    0.4589513024499020D+00, &
   -0.4589513024499019D+00, &
   -0.5838938372432102D+00, &
    0.5838938372432102D+00, &
    0.4855363777625971D+00, &
   -0.4855363777625971D+00, &
    0.1909552287968119D+00, &
   -0.1909552287968118D+00, &
    0.1970910744873101D+00, &
   -0.1970910744873101D+00, &
    0.9070140000742543D+00, &
   -0.9070140000742543D+00, &
   -0.9370706813548184D+00, &
    0.9370706813548186D+00, &
   -0.1024098809482286D+00, &
    0.1024098809482287D+00, &
    0.9018657853563646D+00, &
   -0.9018657853563646D+00, &
    0.7422255782894629D+00, &
   -0.7422255782894629D+00, &
   -0.1975779250586182D-19 /
  data ys / &
   -0.9187170657318696D+00, &
    0.9187170657318696D+00, &
   -0.9679135253250817D+00, &
    0.9679135253250819D+00, &
   -0.9437800394025085D+00, &
    0.9437800394025085D+00, &
   -0.9886578344699537D+00, &
    0.9886578344699537D+00, &
   -0.9803491213417113D+00, &
    0.9803491213417113D+00, &
   -0.8226737868824753D+00, &
    0.8226737868824755D+00, &
   -0.9649601466712245D+00, &
    0.9649601466712245D+00, &
   -0.8370492275539414D+00, &
    0.8370492275539414D+00, &
   -0.9716943047473653D+00, &
    0.9716943047473653D+00, &
   -0.6326447362896030D+00, &
    0.6326447362896030D+00, &
    0.2029425559112923D+00, &
   -0.2029425559112922D+00, &
   -0.7906135688735062D+00, &
    0.7906135688735062D+00, &
   -0.8442560578129694D+00, &
    0.8442560578129694D+00, &
   -0.3117615836793495D+00, &
    0.3117615836793495D+00, &
    0.7701659795648228D+00, &
   -0.7701659795648226D+00, &
   -0.4379432170880169D+00, &
    0.4379432170880170D+00, &
   -0.3820619012323893D+00, &
    0.3820619012323894D+00, &
   -0.6514286057161101D+00, &
    0.6514286057161101D+00, &
   -0.5711068454496305D+00, &
    0.5711068454496305D+00, &
   -0.8072896746317025D-01, &
    0.8072896746317031D-01, &
   -0.8630149364726712D+00, &
    0.8630149364726712D+00, &
   -0.3872678175415290D+00, &
    0.3872678175415290D+00, &
    0.5103334842355030D+00, &
   -0.5103334842355027D+00, &
   -0.9584329986119476D+00, &
    0.9584329986119474D+00, &
   -0.6619201369182062D+00, &
    0.6619201369182062D+00, &
   -0.1238115372273944D+00, &
    0.1238115372273945D+00, &
    0.2071876599633523D+00, &
   -0.2071876599633522D+00, &
    0.5346688849930886D-20 /
  data ws / &
    0.1261638293838951D-01, &
    0.1261638293838951D-01, &
    0.3408339905429266D-02, &
    0.3408339905429266D-02, &
    0.2796862081921473D-01, &
    0.2796862081921473D-01, &
    0.1252812914329644D-01, &
    0.1252812914329644D-01, &
    0.1635296523785200D-01, &
    0.1635296523785200D-01, &
    0.1720881227455075D-01, &
    0.1720881227455075D-01, &
    0.1523407270818440D-01, &
    0.1523407270818440D-01, &
    0.5600796522816800D-01, &
    0.5600796522816800D-01, &
    0.2382823797668716D-01, &
    0.2382823797668716D-01, &
    0.4513279974663867D-01, &
    0.4513279974663867D-01, &
    0.1931215256841166D-01, &
    0.1931215256841166D-01, &
    0.4158804216001467D-01, &
    0.4158804216001467D-01, &
    0.4685849665862760D-01, &
    0.4685849665862760D-01, &
    0.1200522449400290D+00, &
    0.1200522449400290D+00, &
    0.1238565802221201D-01, &
    0.1238565802221201D-01, &
    0.1760077392303538D-01, &
    0.1760077392303538D-01, &
    0.8264937698824523D-01, &
    0.8264937698824523D-01, &
    0.8629133710270168D-01, &
    0.8629133710270168D-01, &
    0.8660536182880048D-01, &
    0.8660536182880048D-01, &
    0.1134857467272575D+00, &
    0.1134857467272575D+00, &
    0.6518861145910534D-01, &
    0.6518861145910534D-01, &
    0.1184802238173896D+00, &
    0.1184802238173896D+00, &
    0.4767526390300979D-01, &
    0.4767526390300979D-01, &
    0.1203076112968188D-01, &
    0.1203076112968188D-01, &
    0.1010849820160845D+00, &
    0.1010849820160845D+00, &
    0.5753445241741756D-01, &
    0.5753445241741756D-01, &
    0.8946701652955226D-01, &
    0.8946701652955226D-01, &
    0.1312734684062163D+00 /

  x(1,1:n) = xs(1:n)
  x(2,1:n) = ys(1:n)
  w(1:ns) = ws(1:ns)

  return
end
subroutine rule18 ( n, x, w )

!*****************************************************************************80
!
!! RULE18 returns the rule of degree 18.
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
!    Output, real ( kind = 8 ) X(2,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 68 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
   -0.9669786385710223D+00, &
    0.9737001842077581D+00, &
    0.9669786385710225D+00, &
   -0.9737001842077578D+00, &
   -0.2156318842512505D+00, &
    0.9910931195695962D+00, &
    0.2156318842512506D+00, &
   -0.9910931195695962D+00, &
   -0.7389660590011030D+00, &
    0.9797385272966153D+00, &
    0.7389660590011032D+00, &
   -0.9797385272966153D+00, &
    0.7689094060317012D+00, &
    0.9882749272572955D+00, &
   -0.7689094060317010D+00, &
   -0.9882749272572955D+00, &
   -0.8922402234413791D+00, &
    0.8925564983087213D+00, &
    0.8922402234413791D+00, &
   -0.8925564983087213D+00, &
    0.2617471442719549D+00, &
    0.9844702542794935D+00, &
   -0.2617471442719548D+00, &
   -0.9844702542794935D+00, &
   -0.7742833119206508D+00, &
    0.7411227454690407D+00, &
    0.7742833119206508D+00, &
   -0.7411227454690407D+00, &
   -0.5506736485553229D+00, &
    0.8796491853095826D+00, &
    0.5506736485553229D+00, &
   -0.8796491853095826D+00, &
   -0.5792562772184127D+00, &
    0.5652337954199163D+00, &
    0.5792562772184127D+00, &
   -0.5652337954199163D+00, &
   -0.1014796206724937D-01, &
    0.9024857168797702D+00, &
    0.1014796206724948D-01, &
   -0.9024857168797702D+00, &
    0.5420066475220151D+00, &
    0.9210890053684702D+00, &
   -0.5420066475220149D+00, &
   -0.9210890053684702D+00, &
    0.2943587054075071D+00, &
    0.7683262972049428D+00, &
   -0.2943587054075070D+00, &
   -0.7683262972049428D+00, &
   -0.3513695172888806D+00, &
    0.3692629613410464D+00, &
    0.3513695172888806D+00, &
   -0.3692629613410464D+00, &
   -0.3707443881794703D+00, &
    0.9667097045148131D+00, &
    0.3707443881794704D+00, &
   -0.9667097045148131D+00, &
   -0.2686897439986438D+00, &
    0.7370294813846769D+00, &
    0.2686897439986439D+00, &
   -0.7370294813846769D+00, &
   -0.1140106895094741D+00, &
    0.1969733705383891D+00, &
    0.1140106895094742D+00, &
   -0.1969733705383891D+00, &
    0.3612358695381902D-01, &
    0.5430113079937613D+00, &
   -0.3612358695381895D-01, &
   -0.5430113079937613D+00 /
  data ys / &
   -0.9737001842077582D+00, &
   -0.9669786385710224D+00, &
    0.9737001842077579D+00, &
    0.9669786385710226D+00, &
   -0.9910931195695962D+00, &
   -0.2156318842512506D+00, &
    0.9910931195695962D+00, &
    0.2156318842512507D+00, &
   -0.9797385272966153D+00, &
   -0.7389660590011031D+00, &
    0.9797385272966153D+00, &
    0.7389660590011033D+00, &
   -0.9882749272572955D+00, &
    0.7689094060317011D+00, &
    0.9882749272572955D+00, &
   -0.7689094060317009D+00, &
   -0.8925564983087213D+00, &
   -0.8922402234413791D+00, &
    0.8925564983087213D+00, &
    0.8922402234413791D+00, &
   -0.9844702542794935D+00, &
    0.2617471442719548D+00, &
    0.9844702542794935D+00, &
   -0.2617471442719547D+00, &
   -0.7411227454690407D+00, &
   -0.7742833119206508D+00, &
    0.7411227454690407D+00, &
    0.7742833119206508D+00, &
   -0.8796491853095826D+00, &
   -0.5506736485553229D+00, &
    0.8796491853095826D+00, &
    0.5506736485553229D+00, &
   -0.5652337954199163D+00, &
   -0.5792562772184127D+00, &
    0.5652337954199163D+00, &
    0.5792562772184127D+00, &
   -0.9024857168797702D+00, &
   -0.1014796206724942D-01, &
    0.9024857168797702D+00, &
    0.1014796206724953D-01, &
   -0.9210890053684702D+00, &
    0.5420066475220150D+00, &
    0.9210890053684702D+00, &
   -0.5420066475220148D+00, &
   -0.7683262972049428D+00, &
    0.2943587054075071D+00, &
    0.7683262972049428D+00, &
   -0.2943587054075070D+00, &
   -0.3692629613410464D+00, &
   -0.3513695172888806D+00, &
    0.3692629613410464D+00, &
    0.3513695172888806D+00, &
   -0.9667097045148131D+00, &
   -0.3707443881794704D+00, &
    0.9667097045148131D+00, &
    0.3707443881794705D+00, &
   -0.7370294813846769D+00, &
   -0.2686897439986438D+00, &
    0.7370294813846769D+00, &
    0.2686897439986439D+00, &
   -0.1969733705383891D+00, &
   -0.1140106895094741D+00, &
    0.1969733705383891D+00, &
    0.1140106895094742D+00, &
   -0.5430113079937613D+00, &
    0.3612358695381898D-01, &
    0.5430113079937613D+00, &
   -0.3612358695381891D-01 /
  data ws / &
    0.4697922862445027D-02, &
    0.4697922862445027D-02, &
    0.4697922862445027D-02, &
    0.4697922862445027D-02, &
    0.7136263254607511D-02, &
    0.7136263254607511D-02, &
    0.7136263254607511D-02, &
    0.7136263254607511D-02, &
    0.1293239065568239D-01, &
    0.1293239065568239D-01, &
    0.1293239065568239D-01, &
    0.1293239065568239D-01, &
    0.9398347568166604D-02, &
    0.9398347568166604D-02, &
    0.9398347568166604D-02, &
    0.9398347568166604D-02, &
    0.1884626577476044D-01, &
    0.1884626577476044D-01, &
    0.1884626577476044D-01, &
    0.1884626577476044D-01, &
    0.1572887987347023D-01, &
    0.1572887987347023D-01, &
    0.1572887987347023D-01, &
    0.1572887987347023D-01, &
    0.4107161379502558D-01, &
    0.4107161379502558D-01, &
    0.4107161379502558D-01, &
    0.4107161379502558D-01, &
    0.4035221395931435D-01, &
    0.4035221395931435D-01, &
    0.4035221395931435D-01, &
    0.4035221395931435D-01, &
    0.6647952625116643D-01, &
    0.6647952625116643D-01, &
    0.6647952625116643D-01, &
    0.6647952625116643D-01, &
    0.4719480581715914D-01, &
    0.4719480581715914D-01, &
    0.4719480581715914D-01, &
    0.4719480581715914D-01, &
    0.3594938959356454D-01, &
    0.3594938959356454D-01, &
    0.3594938959356454D-01, &
    0.3594938959356454D-01, &
    0.6892712069447091D-01, &
    0.6892712069447091D-01, &
    0.6892712069447091D-01, &
    0.6892712069447091D-01, &
    0.8060688072749707D-01, &
    0.8060688072749707D-01, &
    0.8060688072749707D-01, &
    0.8060688072749707D-01, &
    0.1530603725863855D-01, &
    0.1530603725863855D-01, &
    0.1530603725863855D-01, &
    0.1530603725863855D-01, &
    0.7313001882369689D-01, &
    0.7313001882369689D-01, &
    0.7313001882369689D-01, &
    0.7313001882369689D-01, &
    0.7447739831288605D-01, &
    0.7447739831288605D-01, &
    0.7447739831288605D-01, &
    0.7447739831288605D-01, &
    0.9487170596399580D-01, &
    0.9487170596399580D-01, &
    0.9487170596399580D-01, &
    0.9487170596399580D-01 /

  x(1,1:n) = xs(1:n)
  x(2,1:n) = ys(1:n)
  w(1:ns) = ws(1:ns)

  return
end
subroutine rule19 ( n, x, w )

!*****************************************************************************80
!
!! RULE19 returns the rule of degree 19.
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
!    Output, real ( kind = 8 ) X(2,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 68 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
   -0.9734386316165470D+00, &
    0.9744990929036832D+00, &
    0.9734386316165472D+00, &
   -0.9744990929036830D+00, &
   -0.3841574585766744D+00, &
    0.9670641778942685D+00, &
    0.3841574585766745D+00, &
   -0.9670641778942685D+00, &
    0.2986734938364671D+00, &
    0.9905525689050123D+00, &
   -0.2986734938364670D+00, &
   -0.9905525689050123D+00, &
   -0.7396581737067777D+00, &
    0.9869464369033261D+00, &
    0.7396581737067779D+00, &
   -0.9869464369033261D+00, &
   -0.1425244970455050D+00, &
    0.9733021904515969D+00, &
    0.1425244970455051D+00, &
   -0.9733021904515969D+00, &
    0.7650240374639232D+00, &
    0.9804863471920530D+00, &
   -0.7650240374639230D+00, &
   -0.9804863471920530D+00, &
   -0.7599006633708002D+00, &
    0.7279453517455540D+00, &
    0.7599006633708002D+00, &
   -0.7279453517455540D+00, &
   -0.1192987760526789D+00, &
   -0.2637912058730560D-02, &
    0.1192987760526789D+00, &
    0.2637912058730575D-02, &
   -0.8850504442537889D+00, &
    0.9022234232868145D+00, &
    0.8850504442537889D+00, &
   -0.9022234232868145D+00, &
    0.5304174652462883D+00, &
    0.9125489607085608D+00, &
   -0.5304174652462881D+00, &
   -0.9125489607085608D+00, &
   -0.2858528945041368D+00, &
    0.2941600854694212D+00, &
    0.2858528945041368D+00, &
   -0.2941600854694212D+00, &
   -0.5671850101113227D+00, &
    0.8836081660895880D+00, &
    0.5671850101113227D+00, &
   -0.8836081660895880D+00, &
    0.3174295148500719D+00, &
    0.7293427112089215D+00, &
   -0.3174295148500718D+00, &
   -0.7293427112089215D+00, &
   -0.2492430513869149D+00, &
    0.7672563284436533D+00, &
    0.2492430513869150D+00, &
   -0.7672563284436533D+00, &
   -0.5087793568494521D+00, &
    0.5623738439118215D+00, &
    0.5087793568494521D+00, &
   -0.5623738439118215D+00, &
    0.7335719396414396D-01, &
    0.8930925855397183D+00, &
   -0.7335719396414385D-01, &
   -0.8930925855397183D+00, &
    0.8350775723842838D-02, &
    0.5392457387102469D+00, &
   -0.8350775723842772D-02, &
   -0.5392457387102469D+00 /
  data ys / &
   -0.9744990929036833D+00, &
   -0.9734386316165471D+00, &
    0.9744990929036831D+00, &
    0.9734386316165473D+00, &
   -0.9670641778942685D+00, &
   -0.3841574585766744D+00, &
    0.9670641778942685D+00, &
    0.3841574585766745D+00, &
   -0.9905525689050123D+00, &
    0.2986734938364670D+00, &
    0.9905525689050123D+00, &
   -0.2986734938364669D+00, &
   -0.9869464369033261D+00, &
   -0.7396581737067778D+00, &
    0.9869464369033261D+00, &
    0.7396581737067780D+00, &
   -0.9733021904515969D+00, &
   -0.1425244970455050D+00, &
    0.9733021904515969D+00, &
    0.1425244970455051D+00, &
   -0.9804863471920530D+00, &
    0.7650240374639231D+00, &
    0.9804863471920530D+00, &
   -0.7650240374639229D+00, &
   -0.7279453517455540D+00, &
   -0.7599006633708002D+00, &
    0.7279453517455540D+00, &
    0.7599006633708002D+00, &
    0.2637912058730553D-02, &
   -0.1192987760526789D+00, &
   -0.2637912058730568D-02, &
    0.1192987760526789D+00, &
   -0.9022234232868145D+00, &
   -0.8850504442537889D+00, &
    0.9022234232868145D+00, &
    0.8850504442537889D+00, &
   -0.9125489607085608D+00, &
    0.5304174652462882D+00, &
    0.9125489607085608D+00, &
   -0.5304174652462880D+00, &
   -0.2941600854694212D+00, &
   -0.2858528945041368D+00, &
    0.2941600854694212D+00, &
    0.2858528945041368D+00, &
   -0.8836081660895880D+00, &
   -0.5671850101113227D+00, &
    0.8836081660895880D+00, &
    0.5671850101113227D+00, &
   -0.7293427112089215D+00, &
    0.3174295148500719D+00, &
    0.7293427112089215D+00, &
   -0.3174295148500718D+00, &
   -0.7672563284436533D+00, &
   -0.2492430513869149D+00, &
    0.7672563284436533D+00, &
    0.2492430513869150D+00, &
   -0.5623738439118215D+00, &
   -0.5087793568494521D+00, &
    0.5623738439118215D+00, &
    0.5087793568494521D+00, &
   -0.8930925855397183D+00, &
    0.7335719396414390D-01, &
    0.8930925855397183D+00, &
   -0.7335719396414379D-01, &
   -0.5392457387102469D+00, &
    0.8350775723842805D-02, &
    0.5392457387102469D+00, &
   -0.8350775723842739D-02 /
  data ws / &
    0.4076118519980060D-02, &
    0.4076118519980060D-02, &
    0.4076118519980060D-02, &
    0.4076118519980060D-02, &
    0.1627326938099484D-01, &
    0.1627326938099484D-01, &
    0.1627326938099484D-01, &
    0.1627326938099484D-01, &
    0.1254157952509427D-01, &
    0.1254157952509427D-01, &
    0.1254157952509427D-01, &
    0.1254157952509427D-01, &
    0.1028929333936017D-01, &
    0.1028929333936017D-01, &
    0.1028929333936017D-01, &
    0.1028929333936017D-01, &
    0.1475928282295525D-01, &
    0.1475928282295525D-01, &
    0.1475928282295525D-01, &
    0.1475928282295525D-01, &
    0.1207323692393111D-01, &
    0.1207323692393111D-01, &
    0.1207323692393111D-01, &
    0.1207323692393111D-01, &
    0.4619184040692218D-01, &
    0.4619184040692218D-01, &
    0.4619184040692218D-01, &
    0.4619184040692218D-01, &
    0.3696173437828049D-01, &
    0.3696173437828049D-01, &
    0.3696173437828049D-01, &
    0.3696173437828049D-01, &
    0.2018069481193246D-01, &
    0.2018069481193246D-01, &
    0.2018069481193246D-01, &
    0.2018069481193246D-01, &
    0.3738944032940469D-01, &
    0.3738944032940469D-01, &
    0.3738944032940469D-01, &
    0.3738944032940469D-01, &
    0.9821701539315209D-01, &
    0.9821701539315209D-01, &
    0.9821701539315209D-01, &
    0.9821701539315209D-01, &
    0.3844110871724747D-01, &
    0.3844110871724747D-01, &
    0.3844110871724747D-01, &
    0.3844110871724747D-01, &
    0.7127049386881731D-01, &
    0.7127049386881731D-01, &
    0.7127049386881731D-01, &
    0.7127049386881731D-01, &
    0.6966749913838975D-01, &
    0.6966749913838975D-01, &
    0.6966749913838975D-01, &
    0.6966749913838975D-01, &
    0.7715964130310782D-01, &
    0.7715964130310782D-01, &
    0.7715964130310782D-01, &
    0.7715964130310782D-01, &
    0.4598470092336809D-01, &
    0.4598470092336809D-01, &
    0.4598470092336809D-01, &
    0.4598470092336809D-01, &
    0.9562983140360957D-01, &
    0.9562983140360957D-01, &
    0.9562983140360957D-01, &
    0.9562983140360957D-01 /

  x(1,1:n) = xs(1:n)
  x(2,1:n) = ys(1:n)
  w(1:ns) = ws(1:ns)

  return
end
subroutine rule20 ( n, x, w )

!*****************************************************************************80
!
!! RULE20 returns the rule of degree 20.
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
!    Output, real ( kind = 8 ) X(2,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 81 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
   -0.9795110740034025D+00, &
    0.9831906073122737D+00, &
    0.9795110740034028D+00, &
   -0.9831906073122735D+00, &
   -0.7431761069248197D+00, &
    0.9923743096061538D+00, &
    0.7431761069248199D+00, &
   -0.9923743096061538D+00, &
   -0.4283144128355606D+00, &
    0.9641460474769801D+00, &
    0.4283144128355607D+00, &
   -0.9641460474769801D+00, &
    0.2195391124808899D+00, &
    0.9631697483532271D+00, &
   -0.2195391124808898D+00, &
   -0.9631697483532271D+00, &
    0.6056140907858303D+00, &
    0.9331619907848750D+00, &
   -0.6056140907858301D+00, &
   -0.9331619907848750D+00, &
    0.4538625783394974D+00, &
    0.9980174969022684D+00, &
   -0.4538625783394973D+00, &
   -0.9980174969022684D+00, &
   -0.8095537467004988D+00, &
    0.7623591488515665D+00, &
    0.8095537467004988D+00, &
   -0.7623591488515665D+00, &
   -0.1187579119827596D+00, &
    0.9879801664420653D+00, &
    0.1187579119827597D+00, &
   -0.9879801664420653D+00, &
   -0.8923235157505165D+00, &
    0.9333621871500086D+00, &
    0.8923235157505167D+00, &
   -0.9333621871500086D+00, &
    0.8231553038658227D+00, &
    0.9792360167943942D+00, &
   -0.8231553038658225D+00, &
   -0.9792360167943942D+00, &
   -0.2288711050959638D+00, &
    0.8448136056975591D+00, &
    0.2288711050959640D+00, &
   -0.8448136056975591D+00, &
   -0.6414644180013116D+00, &
    0.8887383480333905D+00, &
    0.6414644180013116D+00, &
   -0.8887383480333905D+00, &
    0.2100118285690190D-01, &
    0.9154636292013463D+00, &
   -0.2100118285690179D-01, &
   -0.9154636292013463D+00, &
    0.2939039049089534D+00, &
    0.4700673563865673D+00, &
   -0.2939039049089532D+00, &
   -0.4700673563865673D+00, &
   -0.4701209495753256D+00, &
    0.7110849452816542D+00, &
    0.4701209495753257D+00, &
   -0.7110849452816542D+00, &
   -0.2561845423520469D+00, &
    0.1372468757285573D-01, &
    0.2561845423520469D+00, &
   -0.1372468757285570D-01, &
    0.5331634078426070D+00, &
    0.6746722584255035D+00, &
   -0.5331634078426070D+00, &
   -0.6746722584255035D+00, &
    0.3458330575650539D+00, &
    0.8408056203362516D+00, &
   -0.3458330575650538D+00, &
   -0.8408056203362516D+00, &
    0.6630732857737233D-01, &
    0.6973527543224615D+00, &
   -0.6630732857737225D-01, &
   -0.6973527543224615D+00, &
   -0.2157929992274237D+00, &
    0.5168584327986239D+00, &
    0.2157929992274237D+00, &
   -0.5168584327986239D+00, &
   -0.1195405968452537D-31 /
  data ys / &
   -0.9831906073122738D+00, &
   -0.9795110740034026D+00, &
    0.9831906073122736D+00, &
    0.9795110740034029D+00, &
   -0.9923743096061538D+00, &
   -0.7431761069248198D+00, &
    0.9923743096061538D+00, &
    0.7431761069248201D+00, &
   -0.9641460474769801D+00, &
   -0.4283144128355607D+00, &
    0.9641460474769801D+00, &
    0.4283144128355608D+00, &
   -0.9631697483532271D+00, &
    0.2195391124808899D+00, &
    0.9631697483532271D+00, &
   -0.2195391124808898D+00, &
   -0.9331619907848750D+00, &
    0.6056140907858302D+00, &
    0.9331619907848750D+00, &
   -0.6056140907858300D+00, &
   -0.9980174969022684D+00, &
    0.4538625783394974D+00, &
    0.9980174969022684D+00, &
   -0.4538625783394973D+00, &
   -0.7623591488515665D+00, &
   -0.8095537467004988D+00, &
    0.7623591488515665D+00, &
    0.8095537467004988D+00, &
   -0.9879801664420653D+00, &
   -0.1187579119827596D+00, &
    0.9879801664420653D+00, &
    0.1187579119827597D+00, &
   -0.9333621871500086D+00, &
   -0.8923235157505166D+00, &
    0.9333621871500086D+00, &
    0.8923235157505168D+00, &
   -0.9792360167943942D+00, &
    0.8231553038658226D+00, &
    0.9792360167943942D+00, &
   -0.8231553038658224D+00, &
   -0.8448136056975591D+00, &
   -0.2288711050959639D+00, &
    0.8448136056975591D+00, &
    0.2288711050959640D+00, &
   -0.8887383480333905D+00, &
   -0.6414644180013116D+00, &
    0.8887383480333905D+00, &
    0.6414644180013116D+00, &
   -0.9154636292013463D+00, &
    0.2100118285690184D-01, &
    0.9154636292013463D+00, &
   -0.2100118285690173D-01, &
   -0.4700673563865673D+00, &
    0.2939039049089533D+00, &
    0.4700673563865673D+00, &
   -0.2939039049089532D+00, &
   -0.7110849452816542D+00, &
   -0.4701209495753256D+00, &
    0.7110849452816542D+00, &
    0.4701209495753257D+00, &
   -0.1372468757285574D-01, &
   -0.2561845423520469D+00, &
    0.1372468757285571D-01, &
    0.2561845423520469D+00, &
   -0.6746722584255035D+00, &
    0.5331634078426070D+00, &
    0.6746722584255035D+00, &
   -0.5331634078426070D+00, &
   -0.8408056203362516D+00, &
    0.3458330575650538D+00, &
    0.8408056203362516D+00, &
   -0.3458330575650537D+00, &
   -0.6973527543224615D+00, &
    0.6630732857737229D-01, &
    0.6973527543224615D+00, &
   -0.6630732857737220D-01, &
   -0.5168584327986239D+00, &
   -0.2157929992274237D+00, &
    0.5168584327986239D+00, &
    0.2157929992274238D+00, &
    0.3240416764471269D-32 /
  data ws / &
    0.2403280128020245D-02, &
    0.2403280128020245D-02, &
    0.2403280128020245D-02, &
    0.2403280128020245D-02, &
    0.6918304937846545D-02, &
    0.6918304937846545D-02, &
    0.6918304937846545D-02, &
    0.6918304937846545D-02, &
    0.1998132824455828D-01, &
    0.1998132824455828D-01, &
    0.1998132824455828D-01, &
    0.1998132824455828D-01, &
    0.1612406542082527D-01, &
    0.1612406542082527D-01, &
    0.1612406542082527D-01, &
    0.1612406542082527D-01, &
    0.2451719014395468D-01, &
    0.2451719014395468D-01, &
    0.2451719014395468D-01, &
    0.2451719014395468D-01, &
    0.5618083393401648D-02, &
    0.5618083393401648D-02, &
    0.5618083393401648D-02, &
    0.5618083393401648D-02, &
    0.3267989661107104D-01, &
    0.3267989661107104D-01, &
    0.3267989661107104D-01, &
    0.3267989661107104D-01, &
    0.9643554633385169D-02, &
    0.9643554633385169D-02, &
    0.9643554633385169D-02, &
    0.9643554633385169D-02, &
    0.1438022637487432D-01, &
    0.1438022637487432D-01, &
    0.1438022637487432D-01, &
    0.1438022637487432D-01, &
    0.9462403050575163D-02, &
    0.9462403050575163D-02, &
    0.9462403050575163D-02, &
    0.9462403050575163D-02, &
    0.4414700234043260D-01, &
    0.4414700234043260D-01, &
    0.4414700234043260D-01, &
    0.4414700234043260D-01, &
    0.2997776103642255D-01, &
    0.2997776103642255D-01, &
    0.2997776103642255D-01, &
    0.2997776103642255D-01, &
    0.2217921802120890D-01, &
    0.2217921802120890D-01, &
    0.2217921802120890D-01, &
    0.2217921802120890D-01, &
    0.7979169324002153D-01, &
    0.7979169324002153D-01, &
    0.7979169324002153D-01, &
    0.7979169324002153D-01, &
    0.5450753416951606D-01, &
    0.5450753416951606D-01, &
    0.5450753416951606D-01, &
    0.5450753416951606D-01, &
    0.9164051342923195D-01, &
    0.9164051342923195D-01, &
    0.9164051342923195D-01, &
    0.9164051342923195D-01, &
    0.5417703706712328D-01, &
    0.5417703706712328D-01, &
    0.5417703706712328D-01, &
    0.5417703706712328D-01, &
    0.4265496337854927D-01, &
    0.4265496337854927D-01, &
    0.4265496337854927D-01, &
    0.4265496337854927D-01, &
    0.6713307669025259D-01, &
    0.6713307669025259D-01, &
    0.6713307669025259D-01, &
    0.6713307669025259D-01, &
    0.7903551107191877D-01, &
    0.7903551107191877D-01, &
    0.7903551107191877D-01, &
    0.7903551107191877D-01, &
    0.5365512134302086D-03 /

  x(1,1:n) = xs(1:n)
  x(2,1:n) = ys(1:n)
  w(1:ns) = ws(1:ns)

  return
end
subroutine square_symq ( degree, n, x, w )

!*****************************************************************************80
!
!! SQUARE_SYMQ returns a symmetric quadrature rule for the square.
!
!  Discussion:
!
!    This procedure returns a quadrature rule for smooth functions
!    on the unit square [-1,1]^2.
!
!    All quadratures are accurate to 15 digits
!    All weights are positive and inside the square
!
!    The nodes are symmetrically arranged.
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
!    Input, integer ( kind = 4 ) DEGREE, the degree of the quadrature rule.
!    1 <= DEGREE <= 20.
!
!    Input, integer ( kind = 4 ) N, the number of nodes.
!    This can be determined by a call to RULE_FULL_SIZE(DEGREE).
!
!    Output, real ( kind = 8 ) X(2,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) degree
  real ( kind = 8 ) w(n)
  real ( kind = 8 ) w_sum
  real ( kind = 8 ) x(2,n)

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
  else if ( degree == 16 ) then
    call rule16 ( n, x, w )
  else if ( degree == 17 ) then
    call rule17 ( n, x, w )
  else if ( degree == 18 ) then
    call rule18 ( n, x, w )
  else if ( degree == 19 ) then
    call rule19 ( n, x, w )
  else if ( degree == 20 ) then
    call rule20 ( n, x, w )
  else
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'SQUARE_SYMQ - Fatal error!'
    write ( *, '(a)' ) '  Illegal value of DEGREE.'
    stop 1
  end if

  w_sum = sum ( w(1:n) )

  w(1:n) = 4.0D+00 * w(1:n) / w_sum

  return
end
subroutine square_symq_gnuplot ( n, x, header )

!*****************************************************************************80
!
!! SQUARE_SYMQ_GNUPLOT: GNUPLOT plot of the symmetric square quadrature rule.
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
!    Input, real ( kind = 8 ) X(2,N), the coordinates of the nodes.
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
  real ( kind = 8 ) x(2,n)
!
!  Create the vertex file.
!
  call get_unit ( vertex_unit )
  vertex_filename = trim ( header ) // '_vertices.txt'
  open ( unit = vertex_unit, file = vertex_filename, &
    status = 'replace' )
  write ( vertex_unit, '(g14.6,2x,g14.6)' ) -1.0D+00, -1.0D+00
  write ( vertex_unit, '(g14.6,2x,g14.6)' ) +1.0D+00, -1.0D+00
  write ( vertex_unit, '(g14.6,2x,g14.6)' ) +1.0D+00, +1.0D+00
  write ( vertex_unit, '(g14.6,2x,g14.6)' ) -1.0D+00, +1.0D+00
  write ( vertex_unit, '(g14.6,2x,g14.6)' ) -1.0D+00, -1.0D+00
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
    write ( node_unit, '(g14.6,2x,g14.6)' ) x(1:2,j)
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
  write ( command_unit, '(a)' ) &
    'set title "' // trim ( header ) // '"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set key off'
  write ( command_unit, '(a)' ) 'set size ratio -1'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'set timestamp'
  write ( command_unit, '(a)' ) 'plot "' // &
    trim ( vertex_filename ) // &
    '" with lines lw 3, \'
  write ( command_unit, '(a)' ) '     "' // &
    trim ( node_filename ) // '" with points pt 7 lt 0'
  close ( unit = command_unit )

  write ( *, '(a)' ) &
    '  Created command file "' // trim ( command_filename ) // '".'

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
