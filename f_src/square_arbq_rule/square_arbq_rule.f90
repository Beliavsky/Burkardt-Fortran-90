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
!    06 July 2014
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
      scale = real ( ( 1 + 2 * n ) * ( 1 + 2 * ( m - n ) ), kind = 8 )
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
!    06 July 2014
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
       0.2828427124746189D+01 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
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
!    06 July 2014
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
  parameter ( ns = 3 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
    0.6700319885140564D+00,0.6424528854269665D+00, &
    -.5079273596590297D+00 /
  data ys / &
    -.8727274074699508D+00,0.8751842913892396D+00, &
    -.8014238374817481D-02 /
  data ws / &
    0.6106555690526828D+00,0.6235399890121793D+00, &
    0.1594231566681328D+01 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
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
!    06 July 2014
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
    -.5773502691896256D+00,0.5773502691896260D+00, &
    0.5773502691896256D+00,-.5773502691896260D+00 /
  data ys / &
    -.5773502691896260D+00,-.5773502691896256D+00, &
    0.5773502691896260D+00,0.5773502691896256D+00 /
  data ws / &
    0.7071067811865476D+00,0.7071067811865476D+00, &
    0.7071067811865476D+00,0.7071067811865476D+00 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
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
!    06 July 2014
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
  parameter ( ns = 6 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
    0.8434867168489557D+00,0.7481765370372371D+00, &
    -.4061851500656107D+00,-.4581090172172534D+00, &
    0.1816993641713940D+00,-.9077196977637252D+00 /
  data ys / &
    0.7332250861965538D+00,-.6280294280975105D+00, &
    -.7973798546121016D+00,0.8743017248509551D+00, &
    0.1628466016041256D+00,0.8506794801388022D-02 /
  data ws / &
    0.2754640609309160D+00,0.4372667066134153D+00, &
    0.4966805368802857D+00,0.3707670373943532D+00, &
    0.8675752571634261D+00,0.3806735257637942D+00 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
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
!    06 July 2014
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
    0.1775868202077551D-01,-.1775868202077539D-01, &
    0.7788710544649639D+00,-.7788710544649639D+00, &
    -.7703781288541645D+00,0.7703781288541645D+00, &
    -.7490353914168658D-33 /
  data ys / &
    -.9659285494001192D+00,0.9659285494001192D+00, &
    -.5715708301251639D+00,0.5715708301251639D+00, &
    -.5829672991828014D+00,0.5829672991828014D+00, &
    0.1356144833394667D-33 /
  data ws / &
    0.2246199725165690D+00,0.2246199725165690D+00, &
    0.3901817339168917D+00,0.3901817339168917D+00, &
    0.3953508381187504D+00,0.3953508381187504D+00, &
    0.8081220356417684D+00 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
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
!    06 July 2014
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
  parameter ( ns = 10 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
    -.7347550761673839D+00,0.8662152988634034D+00, &
    0.1596873653424614D+00,-.8905137714296896D+00, &
    -.1469707846748791D+00,0.9240009259977663D+00, &
    -.8463324986375500D+00,-.4086308482879689D+00, &
    0.5175294652720337D+00,0.4801002492857063D+00 /
  data ys / &
    0.8933891941643415D+00,-.7037359670513631D+00, &
    -.9085856749287847D+00,0.1644347368502312D+00, &
    0.5352177835541986D+00,0.4879643750888035D+00, &
    -.8394767448218339D+00,-.4262330870004397D+00, &
    0.9176357850707917D+00,-.1009764561823168D+00 /
  data ws / &
    0.1541850714382379D+00,0.1900556513689156D+00, &
    0.2246942645703077D+00,0.2465847648329768D+00, &
    0.5062382287542438D+00,0.1829226437278864D+00, &
    0.1373586623279704D+00,0.4754388545735908D+00, &
    0.1856913242244974D+00,0.5252576589275637D+00 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
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
    0.4595981103653579D-16,0.9258200997725515D+00, &
    0.6742045114073804D-16,-.9258200997725515D+00, &
    -.3805544332083157D+00,0.3805544332083157D+00, &
    0.3805544332083157D+00,-.3805544332083157D+00, &
    -.8059797829185990D+00,0.8059797829185988D+00, &
    0.8059797829185990D+00,-.8059797829185988D+00 /
  data ys / &
    -.9258200997725515D+00,-.1073032005210112D-16, &
    0.9258200997725515D+00,0.1241105822293750D-15, &
    -.3805544332083157D+00,-.3805544332083157D+00, &
    0.3805544332083157D+00,0.3805544332083157D+00, &
    -.8059797829185988D+00,-.8059797829185990D+00, &
    0.8059797829185988D+00,0.8059797829185990D+00 /
  data ws / &
    0.1711023816204485D+00,0.1711023816204485D+00, &
    0.1711023816204485D+00,0.1711023816204485D+00, &
    0.3681147816131979D+00,0.3681147816131979D+00, &
    0.3681147816131979D+00,0.3681147816131979D+00, &
    0.1678896179529011D+00,0.1678896179529011D+00, &
    0.1678896179529011D+00,0.1678896179529011D+00 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
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
!    06 July 2014
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
  parameter ( ns = 16 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
    -.2272218649369121D+00,0.2786782798124801D+00, &
    0.9215721988395638D+00,-.5229427015551803D+00, &
    0.8309170589376613D+00,-.6080254018462903D+00, &
    -.9822549066167084D+00,0.4959470731361600D-01, &
    0.5910013957537859D+00,0.3626589212754838D+00, &
    -.9369162594801185D+00,-.8850131220663160D+00, &
    -.1934658240272289D+00,0.5772453681919104D+00, &
    0.9213070164035271D+00,-.7176037958340967D+00 /
  data ys / &
    0.8703146041404044D+00,0.9856262640199153D+00, &
    0.2224095500358621D+00,-.9282264259882677D+00, &
    0.8435111761265234D+00,0.5825946042673711D+00, &
    -.8211266831948021D+00,-.6917239446781449D+00, &
    -.2614406969784849D+00,0.5198121135620160D+00, &
    0.2153771996329335D+00,0.9090384216207131D+00, &
    0.3526321874643216D-01,-.9622555555961493D+00, &
    -.7082682674817122D+00,-.4130619139730907D+00 /
  data ws / &
    0.1444235515837947D+00,0.5206905878850610D-01, &
    0.1365819925705312D+00,0.1136963049256808D+00, &
    0.1156201396846171D+00,0.2194056396025883D+00, &
    0.4570142629159132D-01,0.3040158377300561D+00, &
    0.3227772111095287D+00,0.3341175763908440D+00, &
    0.1202823186503543D+00,0.6155232134515501D-01, &
    0.4037250536437860D+00,0.8510021531985533D-01, &
    0.1026971066172272D+00,0.2666613704920739D+00 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
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
!    06 July 2014
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
    0.7502770999789001D+00,-.7502770999789001D+00, &
    -.9279616459595694D+00,0.9279616459595694D+00, &
    0.6306801197316682D+00,-.6306801197316682D+00, &
    0.9688499663619775D+00,-.9688499663619775D+00, &
    -.7620832819261721D-01,0.7620832819261719D-01, &
    0.8526157293336627D+00,-.8526157293336627D+00, &
    0.4533398211356476D+00,-.5237358202144297D+00, &
    0.5237358202144297D+00,-.4533398211356476D+00, &
    -.7154960467453349D-17 /
  data ys / &
    -.9279616459595700D+00,0.9279616459595700D+00, &
    -.7502770999789009D+00,0.7502770999789010D+00, &
    0.9688499663619778D+00,-.9688499663619778D+00, &
    -.6306801197316696D+00,0.6306801197316697D+00, &
    0.8526157293336619D+00,-.8526157293336618D+00, &
    0.7620832819261708D-01,-.7620832819261709D-01, &
    0.5237358202144290D+00,0.4533398211356468D+00, &
    -.4533398211356468D+00,-.5237358202144290D+00, &
    -.1536427274631298D-17 /
  data ws / &
    0.7926638883415150D-01,0.7926638883415155D-01, &
    0.7926638883415171D-01,0.7926638883415174D-01, &
    0.6284721101179108D-01,0.6284721101179105D-01, &
    0.6284721101179126D-01,0.6284721101179125D-01, &
    0.1902480253324007D+00,0.1902480253324007D+00, &
    0.1902480253324001D+00,0.1902480253324001D+00, &
    0.2816282136297291D+00,0.2816282136297291D+00, &
    0.2816282136297291D+00,0.2816282136297291D+00, &
    0.3724677695139016D+00 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
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
!    Input, integer ( kind = 4 ) N, the number of nodes.
!
!    Output, real ( kind = 8 ) X(2,N), the coordinates of the nodes.
!
!    Output, real ( kind = 8 ) W(N), the weights.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) ns
  parameter ( ns = 22 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
     -0.9853833119314600D+00, &
     -0.9262105001258388D+00, &
     -0.9119588710357346D+00, &
     -0.8792348043990323D+00, &
      0.7551269206143556D+00, &
     -0.3188453596839296D+00, &
     -0.6474946981752547D+00, &
      0.9492191314088700D+00, &
     -0.6188661913929927D+00, &
     -0.9215290755789827D+00, &
     -0.1043123255663635D+00, &
      0.9707183739677747D+00, &
      0.9246684242905355D+00, &
     -0.1655785251003832D+00, &
     -0.1844717206212201D+00, &
      0.6666473305982110D+00, &
     -0.6115967830349248D+00, &
      0.3187935759364068D+00, &
      0.3663139167806795D+00, &
      0.2105273891482153D+00, &
      0.7631114939243835D+00, &
      0.6258661935323967D+00 /
  data ys / &
      0.6240243795898477D+00, &
     -0.9577495916000753D+00, &
      0.2065013461988724D+00, &
      0.9225481682574119D+00, &
     -0.9538019223425510D+00, &
      0.9406185571992116D+00, &
      0.6476354842626755D+00, &
      0.1142951736422384D+00, &
     -0.1073322786510873D+00, &
     -0.5089131904296068D+00, &
     -0.9663420836873585D+00, &
     -0.7529656324799600D+00, &
      0.8117151060164874D+00, &
     -0.4732489884927658D+00, &
      0.3507267260891900D+00, &
      0.4711392149070170D+00, &
     -0.8103749226019182D+00, &
     -0.3211002312038632D-01, &
     -0.7605065507139738D+00, &
      0.7788254159831852D+00, &
     -0.3924748753960959D+00, &
      0.9817119264047970D+00 /
  data ws / &
      0.2360826549495683D-01, &
      0.2437222107469133D-01, &
      0.9518348391179431D-01, &
      0.4422100229516362D-01, &
      0.5077443794275623D-01, &
      0.8474079765068482D-01, &
      0.1630376427353708D+00, &
      0.8387536029255430D-01, &
      0.2227307039000182D+00, &
      0.9431200171676321D-01, &
      0.6925434184055250D-01, &
      0.4179721982807350D-01, &
      0.6271061498530471D-01, &
      0.2628440705130901D+00, &
      0.2744383764593098D+00, &
      0.2007889259004832D+00, &
      0.1415509025856471D+00, &
      0.2886706704866435D+00, &
      0.1841475599924024D+00, &
      0.1879992823185209D+00, &
      0.1826311110112870D+00, &
      0.4473813181012295D-01 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
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
!    06 July 2014
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
    0.1885861387186414D+00,0.9535395282015320D+00, &
    -.1885861387186413D+00,-.9535395282015320D+00, &
    -.6980761045495679D+00,0.9826392235408555D+00, &
    0.6980761045495681D+00,-.9826392235408555D+00, &
    -.9394863828167370D+00,0.8257758359029639D+00, &
    0.9394863828167370D+00,-.8257758359029637D+00, &
    -.7120019130753364D+00,0.5253202503645475D+00, &
    0.7120019130753364D+00,-.5253202503645475D+00, &
    -.3156234329152547D+00,0.8125205483048131D+00, &
    0.3156234329152548D+00,-.8125205483048131D+00, &
    -.4248472488486694D+00,0.4165807191202203D-01, &
    0.4248472488486694D+00,-.4165807191202197D-01 /
  data ys / &
    -.9535395282015320D+00,0.1885861387186414D+00, &
    0.9535395282015320D+00,-.1885861387186413D+00, &
    -.9826392235408555D+00,-.6980761045495680D+00, &
    0.9826392235408555D+00,0.6980761045495683D+00, &
    -.8257758359029640D+00,-.9394863828167370D+00, &
    0.8257758359029638D+00,0.9394863828167370D+00, &
    -.5253202503645475D+00,-.7120019130753364D+00, &
    0.5253202503645475D+00,0.7120019130753364D+00, &
    -.8125205483048131D+00,-.3156234329152547D+00, &
    0.8125205483048131D+00,0.3156234329152549D+00, &
    -.4165807191202205D-01,-.4248472488486694D+00, &
    0.4165807191202200D-01,0.4248472488486694D+00/
  data ws / &
    0.6886285066821880D-01,0.6886285066821880D-01, &
    0.6886285066821880D-01,0.6886285066821880D-01, &
    0.3395580740305121D-01,0.3395580740305121D-01, &
    0.3395580740305121D-01,0.3395580740305121D-01, &
    0.4671948489426224D-01,0.4671948489426224D-01, &
    0.4671948489426224D-01,0.4671948489426224D-01, &
    0.1595417182608939D+00,0.1595417182608939D+00, &
    0.1595417182608939D+00,0.1595417182608939D+00, &
    0.1497202089079448D+00,0.1497202089079448D+00, &
    0.1497202089079448D+00,0.1497202089079448D+00, &
    0.2483067110521767D+00,0.2483067110521767D+00, &
    0.2483067110521767D+00,0.2483067110521767D+00 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
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
!    06 July 2014
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
  parameter ( ns = 31 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
    0.9711185107918885D+00,0.6489450771045480D+00, &
    -.9547543104262661D+00,-.9065777988000044D+00, &
    0.9288045791287373D+00,-.9425162358139516D+00, &
    -.9438108523148829D+00,-.6477285885089000D+00, &
    0.9399983037047607D+00,-.6866282782659429D+00, &
    0.7379913501268124D+00,-.3293152288819712D+00, &
    0.6556399582616308D+00,0.4257309111871534D+00, &
    0.9692829476897494D+00,-.8505721097622355D+00, &
    0.2079264382173936D+00,-.8025201782903676D+00, &
    -.5197237466355563D+00,-.2734035281447398D-02, &
    -.3699658428845123D+00,-.6558970744607242D+00, &
    0.3202734978144128D+00,0.8244469498554706D+00, &
    0.2278925123542080D-01,0.5651896934359196D-01, &
    0.4889968338954821D+00,-.3555976156822369D+00, &
    0.7575512967066254D+00,-.1870234315112276D+00, &
    -.9967741042631649D+00 /
  data ys / &
    -.8672105154213969D+00,0.9928922644702000D+00, &
    0.2857493181383339D+00,0.9656011790176721D+00, &
    0.8921207951072256D+00,-.9100219543607504D+00, &
    0.7000393501436398D+00,-.9664634507775797D+00, &
    0.4769006675305678D+00,0.4257467094739614D+00, &
    -.9615153562096814D+00,0.9693604253119810D+00, &
    0.7113042249747283D+00,-.7943285461026974D+00, &
    -.1992840398255900D+00,-.7990773209000775D-01, &
    0.8876704665740045D+00,-.6649760891057823D+00, &
    -.3390779542043381D+00,-.5354471390418425D+00, &
    0.1502820099360215D+00,0.8334029046137713D+00, &
    0.3881559105148546D+00,-.5879856922234445D+00, &
    -.2000991752759776D-01,-.9646721637922943D+00, &
    -.2761642039851812D+00,-.8128294162538594D+00, &
    0.1215344546399007D+00,0.6390274229299343D+00, &
    -.4400036004541968D+00 /
  data ws / &
    0.2294319212922989D-01,0.2269718640167010D-01, &
    0.3814000586151853D-01,0.1921567026521910D-01, &
    0.3433859117158319D-01,0.2503589002871782D-01, &
    0.4151906822977771D-01,0.3384747145223248D-01, &
    0.5960510578836526D-01,0.1273691684426847D+00, &
    0.3629732156183973D-01,0.4288352023218015D-01, &
    0.1186836445978463D+00,0.1223937757234154D+00, &
    0.4775972626669994D-01,0.1037607311404478D+00, &
    0.1017344934330748D+00,0.9441812422392200D-01, &
    0.1662942328844954D+00,0.1752158503094866D+00, &
    0.1535404788337684D+00,0.8331450401650711D-01, &
    0.1951461758691787D+00,0.1055576202902579D+00, &
    0.1749572560557213D+00,0.5131431669876880D-01, &
    0.1804321296492865D+00,0.1127530309084298D+00, &
    0.1400307997981144D+00,0.1721261711453589D+00, &
    0.2510187133639127D-01 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
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
!    06 July 2014
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
    -.9572976997863074D+00,0.8595560056416388D+00, &
    0.9572976997863074D+00,-.8595560056416386D+00, &
    -.7788097115544195D+00,0.9834866824398722D+00, &
    0.7788097115544197D+00,-.9834866824398722D+00, &
    -.4758086252182752D+00,0.8500766736997490D+00, &
    0.4758086252182753D+00,-.8500766736997490D+00, &
    0.3907362161294613D+00,0.9413272258729251D+00, &
    -.3907362161294612D+00,-.9413272258729251D+00, &
    -.1381834598624646D+00,0.9589251702875351D+00, &
    0.1381834598624647D+00,-.9589251702875351D+00, &
    0.6478216371870111D+00,0.7558053565720809D+00, &
    -.6478216371870111D+00,-.7558053565720809D+00, &
    0.7074150899644462D-01,0.6962500784917495D+00, &
    -.7074150899644453D-01,-.6962500784917495D+00, &
    -.3427165560404070D+00,0.4093045616940387D+00, &
    0.3427165560404070D+00,-.4093045616940387D+00, &
    -.7375869198366919D-30 /
  data ys / &
    -.8595560056416389D+00,-.9572976997863074D+00, &
    0.8595560056416387D+00,0.9572976997863074D+00, &
    -.9834866824398722D+00,-.7788097115544196D+00, &
    0.9834866824398722D+00,0.7788097115544198D+00, &
    -.8500766736997490D+00,-.4758086252182752D+00, &
    0.8500766736997490D+00,0.4758086252182753D+00, &
    -.9413272258729251D+00,0.3907362161294612D+00, &
    0.9413272258729251D+00,-.3907362161294611D+00, &
    -.9589251702875351D+00,-.1381834598624647D+00, &
    0.9589251702875351D+00,0.1381834598624648D+00, &
    -.7558053565720809D+00,0.6478216371870111D+00, &
    0.7558053565720809D+00,-.6478216371870111D+00, &
    -.6962500784917495D+00,0.7074150899644457D-01, &
    0.6962500784917495D+00,-.7074150899644449D-01, &
    -.4093045616940387D+00,-.3427165560404070D+00, &
    0.4093045616940387D+00,0.3427165560404070D+00, &
    -.6522588594679827D-30 /
  data ws / &
    0.2699339218118215D-01,0.2699339218118215D-01, &
    0.2699339218118215D-01,0.2699339218118215D-01, &
    0.2120743264134157D-01,0.2120743264134157D-01, &
    0.2120743264134157D-01,0.2120743264134157D-01, &
    0.8403587015611026D-01,0.8403587015611026D-01, &
    0.8403587015611026D-01,0.8403587015611026D-01, &
    0.5479564090947502D-01,0.5479564090947502D-01, &
    0.5479564090947502D-01,0.5479564090947502D-01, &
    0.4272687338421139D-01,0.4272687338421139D-01, &
    0.4272687338421139D-01,0.4272687338421139D-01, &
    0.9175668641747110D-01,0.9175668641747110D-01, &
    0.9175668641747110D-01,0.9175668641747110D-01, &
    0.1508552789574409D+00,0.1508552789574409D+00, &
    0.1508552789574409D+00,0.1508552789574409D+00, &
    0.1816350488471704D+00,0.1816350488471704D+00, &
    0.1816350488471704D+00,0.1816350488471704D+00, &
    0.2124022307685795D+00 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
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
!    06 July 2014
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
  parameter ( ns = 41 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
    -.7971028933442358D-01,0.5471061974611687D+00, &
    0.9884165047157199D-02,-.3207474755493107D+00, &
    -.9553717233098673D-01,0.4082649649717013D+00, &
    0.7282812616257787D+00,-.3472810864715026D+00, &
    -.7915820141518555D+00,0.2785829529075714D+00, &
    0.2099338375706851D+00,-.3770688843422310D+00, &
    0.1546298932128112D+00,0.9706353820201650D+00, &
    -.9710188308157890D+00,-.8089422169597223D-01, &
    -.6907103686706517D+00,-.3766500271820025D+00, &
    -.5730050261123937D+00,0.2628896117741670D+00, &
    0.5452072294160648D+00,-.8660081844945786D+00, &
    0.5293743439059322D+00,0.8326824008480398D+00, &
    -.9838433394477736D+00,0.9724693366798177D+00, &
    0.7239917862034132D+00,-.8848792748044129D+00, &
    0.9060729882068399D+00,0.9965542304973446D+00, &
    -.8318061454455522D+00,-.3342315451147794D+00, &
    0.7013190684905666D+00,0.9589226582728134D+00, &
    -.6067867745349950D+00,0.9099368375229098D+00, &
    -.9529480307145739D+00,-.9784887714287833D+00, &
    -.6288709123199404D+00,-.8984845926717366D+00, &
    0.7834593049247998D+00 /
  data ys / &
    0.7997982556617036D+00,0.8939858342635323D+00, &
    0.4772113760639611D+00,0.7590210924614017D+00, &
    -.1641336121692613D+00,0.6627639082081634D+00, &
    0.9910176287834889D+00,0.1897335228068912D+00, &
    0.1071320357310403D+00,0.1628717488606178D+00, &
    -.4874120187172733D+00,0.9815580199193836D+00, &
    0.9428958078193180D+00,-.9568920474226420D+00, &
    0.2935671028498448D+00,-.8120950161302742D+00, &
    0.8737574543065562D+00,-.5687517308443202D+00, &
    0.5033945159864817D+00,-.9476918771401073D+00, &
    -.1446903403457750D+00,0.6085144943023661D+00, &
    -.7379657281768257D+00,0.7553490523545380D+00, &
    -.7955014974234428D+00,0.5032746878924270D+00, &
    -.9513994070448035D+00,-.9641836389975580D+00, &
    -.7647494365215827D+00,-.4197122679990931D+00, &
    -.5991295673421837D+00,-.9870420901808716D+00, &
    0.3662527369607260D+00,0.9255947941322170D+00, &
    -.2432439926029902D+00,0.2516141162390789D-01, &
    -.2593532979101687D+00,0.8203127557579633D+00, &
    -.8613154366604666D+00,0.9766163862390111D+00, &
    -.4322894664059549D+00 /
  data ws / &
    0.5306119137159240D-01,0.4422789838333461D-01, &
    0.1273937944867034D+00,0.7175684820689888D-01, &
    0.1568138398057906D+00,0.1082893191419578D+00, &
    0.1425381117124736D-01,0.1362424642572790D+00, &
    0.9621728531430029D-01,0.1420579284045241D+00, &
    0.1461917185380448D+00,0.2659569714822968D-01, &
    0.5071663615284024D-01,0.8265110949266337D-02, &
    0.3089045691835443D-01,0.9808292446464849D-01, &
    0.5780479268214644D-01,0.1229597898540734D+00, &
    0.1040216750542093D+00,0.4885605487211036D-01, &
    0.1309068037017440D+00,0.5947734706318328D-01, &
    0.9812697893659589D-01,0.5941729254373463D-01, &
    0.1721117182291158D-01,0.3143615055818434D-01, &
    0.3320596149477782D-01,0.1938593289821144D-01, &
    0.4495089530188040D-01,0.1917919328230456D-01, &
    0.7662013919695503D-01,0.2566629926215708D-01, &
    0.1191871942506123D+00,0.1558050723920070D-01, &
    0.1194301874701882D+00,0.7623831743082932D-01, &
    0.4693631023011145D-01,0.1603574648435874D-01, &
    0.7006443647504916D-01,0.1296824822458430D-01, &
    0.9170277370106396D-01 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
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
!    06 July 2014
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
    0.7749527857778351D+00,0.9885448991378063D+00, &
    -.7749527857778349D+00,-.9885448991378063D+00, &
    -.9070374303651182D+00,0.9571446613308432D+00, &
    0.9070374303651184D+00,-.9571446613308430D+00, &
    -.4303978306869286D+00,0.9769578054468787D+00, &
    0.4303978306869287D+00,-.9769578054468787D+00, &
    -.9756646723906326D+00,0.1107064048513496D+00, &
    0.9756646723906326D+00,-.1107064048513495D+00, &
    -.7388921437312957D+00,0.7868610204187212D+00, &
    0.7388921437312957D+00,-.7868610204187212D+00, &
    0.1995220876718269D+00,0.6659287668239546D+00, &
    -.1995220876718268D+00,-.6659287668239546D+00, &
    -.1934983412061240D+00,0.8412271039808018D+00, &
    0.1934983412061241D+00,-.8412271039808018D+00, &
    0.4882189227791580D+00,0.8922368778153702D+00, &
    -.4882189227791579D+00,-.8922368778153702D+00, &
    -.5772265461040059D+00,0.9526539504944950D+00, &
    0.5772265461040061D+00,-.9526539504944950D+00, &
    -.4474426063114782D+00,0.5675455860909890D+00, &
    0.4474426063114783D+00,-.5675455860909890D+00, &
    -.7044956995149931D-01,0.3256679896817100D+00, &
    0.7044956995149934D-01,-.3256679896817100D+00 /
  data ys / &
    -.9885448991378063D+00,0.7749527857778350D+00, &
    0.9885448991378063D+00,-.7749527857778348D+00, &
    -.9571446613308433D+00,-.9070374303651183D+00, &
    0.9571446613308431D+00,0.9070374303651185D+00, &
    -.9769578054468787D+00,-.4303978306869286D+00, &
    0.9769578054468787D+00,0.4303978306869287D+00, &
    -.1107064048513496D+00,-.9756646723906326D+00, &
    0.1107064048513495D+00,0.9756646723906326D+00, &
    -.7868610204187212D+00,-.7388921437312957D+00, &
    0.7868610204187212D+00,0.7388921437312957D+00, &
    -.6659287668239546D+00,0.1995220876718268D+00, &
    0.6659287668239546D+00,-.1995220876718268D+00, &
    -.8412271039808018D+00,-.1934983412061240D+00, &
    0.8412271039808018D+00,0.1934983412061241D+00, &
    -.8922368778153702D+00,0.4882189227791580D+00, &
    0.8922368778153702D+00,-.4882189227791578D+00, &
    -.9526539504944950D+00,-.5772265461040060D+00, &
    0.9526539504944950D+00,0.5772265461040063D+00, &
    -.5675455860909890D+00,-.4474426063114783D+00, &
    0.5675455860909890D+00,0.4474426063114784D+00, &
    -.3256679896817100D+00,-.7044956995149933D-01, &
    0.3256679896817100D+00,0.7044956995149936D-01 /
  data ws / &
    0.1443015463807196D-01,0.1443015463807196D-01, &
    0.1443015463807196D-01,0.1443015463807196D-01, &
    0.1816242330920956D-01,0.1816242330920956D-01, &
    0.1816242330920956D-01,0.1816242330920956D-01, &
    0.1290815898308381D-01,0.1290815898308381D-01, &
    0.1290815898308381D-01,0.1290815898308381D-01, &
    0.3010764365372140D-01,0.3010764365372140D-01, &
    0.3010764365372140D-01,0.3010764365372140D-01, &
    0.6540469907131932D-01,0.6540469907131932D-01, &
    0.6540469907131932D-01,0.6540469907131932D-01, &
    0.1197895531736646D+00,0.1197895531736646D+00, &
    0.1197895531736646D+00,0.1197895531736646D+00, &
    0.8473841548096289D-01,0.8473841548096289D-01, &
    0.8473841548096289D-01,0.8473841548096289D-01, &
    0.6453833756714425D-01,0.6453833756714425D-01, &
    0.6453833756714425D-01,0.6453833756714425D-01, &
    0.2403055376316494D-01,0.2403055376316494D-01, &
    0.2403055376316494D-01,0.2403055376316494D-01, &
    0.1196130510491228D+00,0.1196130510491228D+00, &
    0.1196130510491228D+00,0.1196130510491228D+00, &
    0.1533837904970821D+00,0.1533837904970821D+00, &
    0.1533837904970821D+00,0.1533837904970821D+00 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
  w(1:n) = ws(1:n)

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
!    06 July 2014
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
  parameter ( ns = 52 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
    0.4455077315117606D+00,-.2848639978436560D+00, &
    0.3281612731066613D+00,0.8918323090897292D+00, &
    0.5080739455207711D+00,-.1604014838336734D+00, &
    -.5084786013975685D+00,0.6830501661618820D+00, &
    0.3958609078545774D-01,-.9505884228486957D+00, &
    -.9618117119546932D+00,-.9824876605856204D+00, &
    0.7294909884375445D+00,0.1663512176814138D+00, &
    -.8401346406026955D+00,0.7196828159773350D+00, &
    0.2530272226250717D+00,0.6403128558574714D-01, &
    0.9867649245062882D+00,-.2009570267598735D+00, &
    -.4385685045405642D+00,0.9942720499027967D+00, &
    -.8306577686854565D+00,0.1540093368841131D+00, &
    0.5127230397362322D+00,-.9896697019211779D+00, &
    0.9721677355950014D+00,-.7432189863550824D+00, &
    0.7845206176595362D+00,0.9792382924978336D+00, &
    -.7071472873150695D+00,-.1416891896032315D+00, &
    0.8901118844470474D+00,0.8991291622644930D+00, &
    -.8948787586840297D+00,-.1685236423690618D+00, &
    0.8814614259143865D+00,-.9727794481100781D+00, &
    -.2335640652756550D+00,0.9317484014546712D+00, &
    0.4462548323794501D+00,-.4956609448877936D+00, &
    -.7464462325957708D+00,-.6209288757442605D+00, &
    -.9729904625306640D+00,0.6851689211463186D+00, &
    -.6950237982778372D+00,0.1406973647399049D+00, &
    -.8903519744155600D+00,-.4494459013630722D+00, &
    0.6393855383975146D+00,0.4185604687784284D+00 /
  data ys / &
    -.4133544718603057D+00,-.7739045957079853D+00, &
    -.9418618376442869D+00,-.5606355473579230D+00, &
    0.9698717423735355D+00,-.4892197731244818D+00, &
    -.9117050044897801D+00,-.5146278686624721D+00, &
    -.8810600362311165D+00,-.9545711191201147D+00, &
    -.5466258413042109D+00,-.8038321990753307D+00, &
    -.1738438525238659D+00,-.6521123405187266D+00, &
    -.3199762775663758D+00,0.8583352444264194D+00, &
    0.9559713160961977D+00,0.8638442022353128D+00, &
    -.7782646945578277D+00,0.6577321912732575D+00, &
    -.2429524974731521D+00,0.3876054361677954D+00, &
    -.7951217128043849D+00,-.2163105603296342D+00, &
    -.7522598930908062D+00,0.5914422527084512D+00, &
    -.2390927720573675D+00,-.9749994483165604D+00, &
    -.8444622014821822D+00,0.8810775159513397D+00, &
    0.6173187727164633D+00,0.7292324652124864D-01, &
    0.1389731666217061D+00,0.6539121663076461D+00, &
    0.8152409648202001D+00,-.9939582277051608D+00, &
    0.9818676808679303D+00,0.9582010007957961D+00, &
    0.9824162852107247D+00,-.9611203598796459D+00, &
    0.1014796681115945D+00,0.8630083224142983D+00, &
    0.9693553936129727D+00,-.5969027498090264D+00, &
    -.3789880325419188D-01,0.4272704958652744D+00, &
    0.4313302857584844D-01,0.4101941952194764D+00, &
    0.3277950128817075D+00,0.3540266909627898D+00, &
    -.9786311482371555D+00,0.6960968466161754D+00 /
  data ws / &
    0.7760560264177564D-01,0.6557384620049388D-01, &
    0.3492505367961147D-01,0.4902394774171961D-01, &
    0.2071780611220039D-01,0.9663554422001587D-01, &
    0.4275428371393922D-01,0.4251351999693562D-01, &
    0.4614134112945410D-01,0.1008804262208153D-01, &
    0.2597909269494542D-01,0.7243920128199269D-02, &
    0.8689727864752500D-01,0.7770693383996630D-01, &
    0.6824785727785596D-01,0.4942860806128915D-01, &
    0.1939352812224098D-01,0.6287664325857020D-01, &
    0.1228661441972498D-01,0.1029914685037788D+00, &
    0.1129049857606719D+00,0.1469344234054041D-01, &
    0.4431063501362172D-01,0.1183113207778475D+00, &
    0.6355581532812171D-01,0.1527602237522992D-01, &
    0.2841229216361006D-01,0.1472432061291935D-01, &
    0.4129113754659276D-01,0.1149253014935389D-01, &
    0.7883123749996855D-01,0.1339728049062325D+00, &
    0.6215863520566767D-01,0.4742159015437373D-01, &
    0.3530277887828205D-01,0.1663690106625590D-01, &
    0.1074607484017106D-01,0.6944512701761030D-02, &
    0.2298755190493018D-01,0.1230850655669064D-01, &
    0.1267827808948161D+00,0.6295607382580996D-01, &
    0.2066068139152711D-01,0.8653018393026629D-01, &
    0.2938486912464724D-01,0.9600456501593493D-01, &
    0.9622610521466571D-01,0.1308302132754554D+00, &
    0.6115363632570159D-01,0.1144365242523608D+00, &
    0.1628247685682751D-01,0.9586498584301183D-01 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
  w(1:n) = ws(1:n)

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
!    06 July 2014
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
    -.7710386602263628D+00,0.7710386602263630D+00, &
    0.9803457456469387D+00,-.9803457456469384D+00, &
    -.2292639639675523D+00,0.2292639639675524D+00, &
    0.4847176019505991D-03,-.4847176019504780D-03, &
    -.6189416389750175D+00,0.6189416389750177D+00, &
    0.9587315519802511D+00,-.9587315519802511D+00, &
    0.8409306922533750D+00,-.8409306922533748D+00, &
    -.4308042054877432D+00,0.4308042054877433D+00, &
    0.4761431266211590D+00,-.4761431266211589D+00, &
    0.8651144531733139D+00,-.8651144531733139D+00, &
    0.9846617345267017D+00,-.9846617345267017D+00, &
    -.7981639404863030D+00,0.7981639404863030D+00, &
    0.6877591943414725D+00,-.6877591943414725D+00, &
    -.3038305486106544D+00,0.3038305486106544D+00, &
    0.9852576255116258D+00,-.9852576255116258D+00, &
    0.9853756930046446D+00,-.9853756930046446D+00, &
    0.7024672194580522D+00,-.7024672194580522D+00, &
    0.4589513024499020D+00,-.4589513024499019D+00, &
    -.5838938372432102D+00,0.5838938372432102D+00, &
    0.4855363777625971D+00,-.4855363777625971D+00, &
    0.1909552287968119D+00,-.1909552287968118D+00, &
    0.1970910744873101D+00,-.1970910744873101D+00, &
    0.9070140000742543D+00,-.9070140000742543D+00, &
    -.9370706813548184D+00,0.9370706813548186D+00, &
    -.1024098809482286D+00,0.1024098809482287D+00, &
    0.9018657853563646D+00,-.9018657853563646D+00, &
    0.7422255782894629D+00,-.7422255782894629D+00, &
    -.1975779250586182D-19 /
  data ys / &
    -.9187170657318696D+00,0.9187170657318696D+00, &
    -.9679135253250817D+00,0.9679135253250819D+00, &
    -.9437800394025085D+00,0.9437800394025085D+00, &
    -.9886578344699537D+00,0.9886578344699537D+00, &
    -.9803491213417113D+00,0.9803491213417113D+00, &
    -.8226737868824753D+00,0.8226737868824755D+00, &
    -.9649601466712245D+00,0.9649601466712245D+00, &
    -.8370492275539414D+00,0.8370492275539414D+00, &
    -.9716943047473653D+00,0.9716943047473653D+00, &
    -.6326447362896030D+00,0.6326447362896030D+00, &
    0.2029425559112923D+00,-.2029425559112922D+00, &
    -.7906135688735062D+00,0.7906135688735062D+00, &
    -.8442560578129694D+00,0.8442560578129694D+00, &
    -.3117615836793495D+00,0.3117615836793495D+00, &
    0.7701659795648228D+00,-.7701659795648226D+00, &
    -.4379432170880169D+00,0.4379432170880170D+00, &
    -.3820619012323893D+00,0.3820619012323894D+00, &
    -.6514286057161101D+00,0.6514286057161101D+00, &
    -.5711068454496305D+00,0.5711068454496305D+00, &
    -.8072896746317025D-01,0.8072896746317031D-01, &
    -.8630149364726712D+00,0.8630149364726712D+00, &
    -.3872678175415290D+00,0.3872678175415290D+00, &
    0.5103334842355030D+00,-.5103334842355027D+00, &
    -.9584329986119476D+00,0.9584329986119474D+00, &
    -.6619201369182062D+00,0.6619201369182062D+00, &
    -.1238115372273944D+00,0.1238115372273945D+00, &
    0.2071876599633523D+00,-.2071876599633522D+00, &
    0.5346688849930886D-20 /
  data ws / &
    0.1261638293838951D-01,0.1261638293838951D-01, &
    0.3408339905429266D-02,0.3408339905429266D-02, &
    0.2796862081921473D-01,0.2796862081921473D-01, &
    0.1252812914329644D-01,0.1252812914329644D-01, &
    0.1635296523785200D-01,0.1635296523785200D-01, &
    0.1720881227455075D-01,0.1720881227455075D-01, &
    0.1523407270818440D-01,0.1523407270818440D-01, &
    0.5600796522816800D-01,0.5600796522816800D-01, &
    0.2382823797668716D-01,0.2382823797668716D-01, &
    0.4513279974663867D-01,0.4513279974663867D-01, &
    0.1931215256841166D-01,0.1931215256841166D-01, &
    0.4158804216001467D-01,0.4158804216001467D-01, &
    0.4685849665862760D-01,0.4685849665862760D-01, &
    0.1200522449400290D+00,0.1200522449400290D+00, &
    0.1238565802221201D-01,0.1238565802221201D-01, &
    0.1760077392303538D-01,0.1760077392303538D-01, &
    0.8264937698824523D-01,0.8264937698824523D-01, &
    0.8629133710270168D-01,0.8629133710270168D-01, &
    0.8660536182880048D-01,0.8660536182880048D-01, &
    0.1134857467272575D+00,0.1134857467272575D+00, &
    0.6518861145910534D-01,0.6518861145910534D-01, &
    0.1184802238173896D+00,0.1184802238173896D+00, &
    0.4767526390300979D-01,0.4767526390300979D-01, &
    0.1203076112968188D-01,0.1203076112968188D-01, &
    0.1010849820160845D+00,0.1010849820160845D+00, &
    0.5753445241741756D-01,0.5753445241741756D-01, &
    0.8946701652955226D-01,0.8946701652955226D-01, &
    0.1312734684062163D+00 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
  w(1:n) = ws(1:n)

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
!    06 July 2014
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
  parameter ( ns = 64 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
    -.9824292972819758D+00,-.9803800150830472D+00, &
    0.9786873970954008D+00,0.9783064717068459D+00, &
    0.9721036514650782D+00,0.5934299192489508D+00, &
    0.5976341012197494D+00,-.3692961886011435D+00, &
    0.7049479055795181D+00,-.7373468008004338D+00, &
    0.9861989064660380D+00,-.8465158209874328D-01, &
    0.8850139974482023D+00,-.9313926150284875D+00, &
    -.7089981957319805D+00,0.8732614092173598D+00, &
    -.8419399215622921D+00,0.3224279462028189D+00, &
    0.2244640124522700D+00,-.8347180004932071D+00, &
    -.8279536304784142D+00,-.6568125381001307D+00, &
    -.8460042964230963D+00,-.9242174294702851D+00, &
    -.5414368844899894D+00,0.9265304841706828D+00, &
    -.8981156173043389D+00,-.4115732120271069D+00, &
    0.6047453622518654D+00,-.5983900835098156D+00, &
    0.4320397790648277D+00,0.4499667234152572D+00, &
    0.9513764564603604D+00,-.6795979700947666D+00, &
    -.6564652118524049D+00,0.8378047852517292D+00, &
    -.1113556772390856D+00,-.3608746962710803D+00, &
    0.9862753904173321D+00,-.2331918978886594D+00, &
    -.9870492227974196D+00,-.9894160854750882D+00, &
    -.9836502238795436D+00,0.8002432442611336D+00, &
    0.6492404239850669D+00,0.8701160725699196D+00, &
    0.7389321385106327D+00,-.4715855030569622D+00, &
    0.7436165358202136D+00,0.3534834684769252D+00, &
    -.3650849452075164D+00,-.2283506208713122D+00, &
    0.1740274102467612D+00,0.8995839783500573D+00, &
    0.3405925655026003D+00,0.4295328792252409D-01, &
    0.1448653016063912D+00,-.1312827821241097D+00, &
    0.5004638295686703D+00,0.6006609259220026D-01, &
    0.1552257232885370D+00,-.9395864632354990D+00, &
    -.4791507758959901D+00,-.8137902397325638D+00 /
  data ys / &
    -.9492304540136947D+00,0.9469609026131530D+00, &
    0.7622186219256711D+00,-.9196983748762729D+00, &
    0.9764183470032382D+00,-.9616466720526158D+00, &
    0.8343589711117383D+00,0.9800036938552533D+00, &
    0.9870747330066914D+00,-.8333460482416958D+00, &
    -.3278737740897394D+00,0.8908466843089227D+00, &
    -.9855803626832146D+00,-.8212845135474046D+00, &
    0.6822143912173052D+00,0.9031148761319726D+00, &
    0.9885786707367459D+00,0.5365154393395204D-01, &
    0.7350437600270140D+00,0.5084609108470370D+00, &
    -.6132315128115761D+00,0.2290780703350274D+00, &
    -.9786418155875946D+00,-.3102577845761006D+00, &
    -.6275098391746888D+00,0.1407483577400233D-01, &
    0.8295680022967102D+00,0.7753196502785407D+00, &
    -.6526316599009200D+00,-.9333674622588783D+00, &
    0.9243432783386737D+00,0.7005646989935736D-01, &
    -.6925969506257750D+00,-.3641945758998874D+00, &
    0.9193665018045939D+00,-.4639915525654867D+00, &
    -.9041936920710849D+00,-.7808865818750257D+00, &
    0.2974467982752778D+00,-.4343746051515523D+00, &
    -.5898322990766285D+00,-.1604383366455782D-01, &
    0.6393477839238390D+00,-.8481629731994191D+00, &
    -.2054507717193144D+00,-.1017355764563722D+00, &
    0.7159568279790096D+00,-.1245817088489083D+00, &
    0.2542532076804419D+00,-.8415199898068800D+00, &
    -.9897219383699781D+00,0.1556384545656453D+00, &
    -.9791074324031704D+00,0.5295267411341480D+00, &
    -.4165391069331053D+00,-.1509802365687459D+00, &
    0.3347376662738560D+00,0.5759504751299614D+00, &
    0.5086985983019631D+00,-.6661951769904182D+00, &
    0.9847264496502689D+00,0.3003342534564235D+00, &
    0.4492326697835803D+00,-.6261874651130582D-02/
  data ws / &
    0.4880182878194577D-02,0.5801719791846293D-02, &
    0.1363202620128554D-01,0.7756485561516194D-02, &
    0.4249395346845679D-02,0.2449228653823736D-01, &
    0.2751148878355652D-01,0.1865262619502059D-01, &
    0.1160827955745764D-01,0.2909451783554787D-01, &
    0.1614009453230349D-01,0.5169172255655628D-01, &
    0.7388591103278081D-02,0.2021700469490708D-01, &
    0.4636019735255641D-01,0.2315518049910366D-01, &
    0.7815468109974862D-02,0.3769198715796499D-01, &
    0.7881010252197586D-01,0.3988856078693273D-01, &
    0.4079805534552380D-01,0.6645673137795649D-01, &
    0.1072546708888146D-01,0.3868425989902273D-01, &
    0.5522501374016728D-01,0.3253997663365805D-01, &
    0.2752684329481517D-01,0.6117844466269161D-01, &
    0.7168779734796255D-01,0.2620423047578233D-01, &
    0.3392741843404458D-01,0.7458228761237336D-01, &
    0.2478342723311915D-01,0.6447492653722675D-01, &
    0.3284650513806075D-01,0.5368779603413000D-01, &
    0.4545538155398856D-01,0.5397016565409892D-01, &
    0.1519721989624682D-01,0.1031663549936232D+00, &
    0.1231416020827792D-01,0.1349682430871745D-01, &
    0.1395336957267540D-01,0.3753801076980155D-01, &
    0.8258232039188609D-01,0.2489123627268648D-01, &
    0.4546454333835988D-01,0.9452263223987649D-01, &
    0.7842163445187621D-01,0.5876926206730445D-01, &
    0.1249000951778932D-01,0.1088923354481117D+00, &
    0.1951366247998040D-01,0.4179385297531394D-01, &
    0.1033273611701436D+00,0.1162365062083885D+00, &
    0.1023818705273872D+00,0.8804476252208088D-01, &
    0.8910543971624120D-01,0.8874335328605459D-01, &
    0.1602722062379702D-01,0.3378674145303614D-01, &
    0.7595232871119993D-01,0.6022146552676879D-01 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
  w(1:n) = ws(1:n)

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
!    06 July 2014
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
    -.9734386316165470D+00,0.9744990929036832D+00, &
    0.9734386316165472D+00,-.9744990929036830D+00, &
    -.3841574585766744D+00,0.9670641778942685D+00, &
    0.3841574585766745D+00,-.9670641778942685D+00, &
    0.2986734938364671D+00,0.9905525689050123D+00, &
    -.2986734938364670D+00,-.9905525689050123D+00, &
    -.7396581737067777D+00,0.9869464369033261D+00, &
    0.7396581737067779D+00,-.9869464369033261D+00, &
    -.1425244970455050D+00,0.9733021904515969D+00, &
    0.1425244970455051D+00,-.9733021904515969D+00, &
    0.7650240374639232D+00,0.9804863471920530D+00, &
    -.7650240374639230D+00,-.9804863471920530D+00, &
    -.7599006633708002D+00,0.7279453517455540D+00, &
    0.7599006633708002D+00,-.7279453517455540D+00, &
    -.1192987760526789D+00,-.2637912058730560D-02, &
    0.1192987760526789D+00,0.2637912058730575D-02, &
    -.8850504442537889D+00,0.9022234232868145D+00, &
    0.8850504442537889D+00,-.9022234232868145D+00, &
    0.5304174652462883D+00,0.9125489607085608D+00, &
    -.5304174652462881D+00,-.9125489607085608D+00, &
    -.2858528945041368D+00,0.2941600854694212D+00, &
    0.2858528945041368D+00,-.2941600854694212D+00, &
    -.5671850101113227D+00,0.8836081660895880D+00, &
    0.5671850101113227D+00,-.8836081660895880D+00, &
    0.3174295148500719D+00,0.7293427112089215D+00, &
    -.3174295148500718D+00,-.7293427112089215D+00, &
    -.2492430513869149D+00,0.7672563284436533D+00, &
    0.2492430513869150D+00,-.7672563284436533D+00, &
    -.5087793568494521D+00,0.5623738439118215D+00, &
    0.5087793568494521D+00,-.5623738439118215D+00, &
    0.7335719396414396D-01,0.8930925855397183D+00, &
    -.7335719396414385D-01,-.8930925855397183D+00, &
    0.8350775723842838D-02,0.5392457387102469D+00, &
    -.8350775723842772D-02,-.5392457387102469D+00 /
  data ys / &
    -.9744990929036833D+00,-.9734386316165471D+00, &
    0.9744990929036831D+00,0.9734386316165473D+00, &
    -.9670641778942685D+00,-.3841574585766744D+00, &
    0.9670641778942685D+00,0.3841574585766745D+00, &
    -.9905525689050123D+00,0.2986734938364670D+00, &
    0.9905525689050123D+00,-.2986734938364669D+00, &
    -.9869464369033261D+00,-.7396581737067778D+00, &
    0.9869464369033261D+00,0.7396581737067780D+00, &
    -.9733021904515969D+00,-.1425244970455050D+00, &
    0.9733021904515969D+00,0.1425244970455051D+00, &
    -.9804863471920530D+00,0.7650240374639231D+00, &
    0.9804863471920530D+00,-.7650240374639229D+00, &
    -.7279453517455540D+00,-.7599006633708002D+00, &
    0.7279453517455540D+00,0.7599006633708002D+00, &
    0.2637912058730553D-02,-.1192987760526789D+00, &
    -.2637912058730568D-02,0.1192987760526789D+00, &
    -.9022234232868145D+00,-.8850504442537889D+00, &
    0.9022234232868145D+00,0.8850504442537889D+00, &
    -.9125489607085608D+00,0.5304174652462882D+00, &
    0.9125489607085608D+00,-.5304174652462880D+00, &
    -.2941600854694212D+00,-.2858528945041368D+00, &
    0.2941600854694212D+00,0.2858528945041368D+00, &
    -.8836081660895880D+00,-.5671850101113227D+00, &
    0.8836081660895880D+00,0.5671850101113227D+00, &
    -.7293427112089215D+00,0.3174295148500719D+00, &
    0.7293427112089215D+00,-.3174295148500718D+00, &
    -.7672563284436533D+00,-.2492430513869149D+00, &
    0.7672563284436533D+00,0.2492430513869150D+00, &
    -.5623738439118215D+00,-.5087793568494521D+00, &
    0.5623738439118215D+00,0.5087793568494521D+00, &
    -.8930925855397183D+00,0.7335719396414390D-01, &
    0.8930925855397183D+00,-.7335719396414379D-01, &
    -.5392457387102469D+00,0.8350775723842805D-02, &
    0.5392457387102469D+00,-.8350775723842739D-02 /
  data ws / &
    0.4076118519980060D-02,0.4076118519980060D-02, &
    0.4076118519980060D-02,0.4076118519980060D-02, &
    0.1627326938099484D-01,0.1627326938099484D-01, &
    0.1627326938099484D-01,0.1627326938099484D-01, &
    0.1254157952509427D-01,0.1254157952509427D-01, &
    0.1254157952509427D-01,0.1254157952509427D-01, &
    0.1028929333936017D-01,0.1028929333936017D-01, &
    0.1028929333936017D-01,0.1028929333936017D-01, &
    0.1475928282295525D-01,0.1475928282295525D-01, &
    0.1475928282295525D-01,0.1475928282295525D-01, &
    0.1207323692393111D-01,0.1207323692393111D-01, &
    0.1207323692393111D-01,0.1207323692393111D-01, &
    0.4619184040692218D-01,0.4619184040692218D-01, &
    0.4619184040692218D-01,0.4619184040692218D-01, &
    0.3696173437828049D-01,0.3696173437828049D-01, &
    0.3696173437828049D-01,0.3696173437828049D-01, &
    0.2018069481193246D-01,0.2018069481193246D-01, &
    0.2018069481193246D-01,0.2018069481193246D-01, &
    0.3738944032940469D-01,0.3738944032940469D-01, &
    0.3738944032940469D-01,0.3738944032940469D-01, &
    0.9821701539315209D-01,0.9821701539315209D-01, &
    0.9821701539315209D-01,0.9821701539315209D-01, &
    0.3844110871724747D-01,0.3844110871724747D-01, &
    0.3844110871724747D-01,0.3844110871724747D-01, &
    0.7127049386881731D-01,0.7127049386881731D-01, &
    0.7127049386881731D-01,0.7127049386881731D-01, &
    0.6966749913838975D-01,0.6966749913838975D-01, &
    0.6966749913838975D-01,0.6966749913838975D-01, &
    0.7715964130310782D-01,0.7715964130310782D-01, &
    0.7715964130310782D-01,0.7715964130310782D-01, &
    0.4598470092336809D-01,0.4598470092336809D-01, &
    0.4598470092336809D-01,0.4598470092336809D-01, &
    0.9562983140360957D-01,0.9562983140360957D-01, &
    0.9562983140360957D-01,0.9562983140360957D-01 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
  w(1:n) = ws(1:n)

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
!    06 July 2014
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
  parameter ( ns = 78 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
    0.7903555901298286D+00,0.2252273789362463D+00, &
    -.1371556203352229D+00,0.9516912467651903D+00, &
    0.8981438613797404D+00,0.3511327336932318D+00, &
    0.9759068343062448D-01,-.1920781702210826D+00, &
    0.6695436306885109D+00,0.5830289505702392D+00, &
    -.4278190227205511D+00,0.5727845316573248D-01, &
    0.7351507200926066D+00,0.7898934964234535D+00, &
    -.3594399777482915D+00,0.3376839183112454D+00, &
    0.9886396714914109D+00,-.3907721596661082D+00, &
    -.6046899139754487D+00,0.9173007153030285D+00, &
    -.4141396903659944D-01,-.6385982895810176D+00, &
    0.6013396134438070D+00,0.9161661661998538D+00, &
    -.3278639859221721D+00,0.8802460227981840D+00, &
    -.2034581078060147D+00,0.6969166399839279D+00, &
    0.5351593293803870D+00,-.2787747819837111D+00, &
    0.9910314479371701D+00,-.8252889264259545D+00, &
    -.9599491206891601D+00,0.4944916835714113D+00, &
    -.7703896716180660D+00,0.9129604837711394D+00, &
    0.8500373866499284D+00,-.3755453354695931D-01, &
    -.9734574105217517D+00,0.2897016770313429D+00, &
    0.7688438290660360D+00,-.8388618516990838D+00, &
    -.5341286913684105D+00,-.9174806831430735D+00, &
    -.9474346838846874D+00,-.6131936263177837D-01, &
    0.7605933106317154D+00,-.9848886462575595D+00, &
    -.7572987820459326D+00,0.5080745682281066D-01, &
    -.4636550029355001D+00,0.9705319244227489D+00, &
    -.8914079947263784D+00,-.9873056954830064D+00, &
    -.9758313372889643D+00,-.8819170869186215D+00, &
    -.9709635895278415D+00,0.9822722262702339D+00, &
    -.8873503942891041D+00,-.6569247082044029D+00, &
    -.9870905373622706D+00,-.5734674551654146D+00, &
    0.1507888619083396D-01,0.5506050560553492D+00, &
    -.7366839885772312D+00,-.7117026556332434D+00, &
    0.2333965922395885D+00,-.2878681152474931D+00, &
    0.4361746990352844D+00,0.9814633895599361D+00, &
    -.5319601634991685D+00,0.9850987522022563D+00, &
    0.1957972824970721D+00,0.2084217400195901D+00, &
    0.4755027720742199D+00,0.9147494843686150D+00, &
    -.8645689328165518D+00,0.6772212070927294D+00 /
  data ys / &
    -.8898248168845113D-01,-.7280953904485143D-01, &
    -.8653524201560835D+00,-.2902346585197535D+00, &
    0.1218660048097990D+00,-.2471221282702101D+00, &
    -.7467915296200941D+00,-.5589830789081398D+00, &
    -.2090651941599068D+00,-.4355704406099477D+00, &
    -.7264771605644266D+00,-.1363590388950549D+00, &
    -.9878512802669168D+00,0.9209737732078764D+00, &
    -.9535217391501140D+00,-.5953361238026516D+00, &
    -.7515863705429210D-01,0.9865491452054407D+00, &
    -.9940939029178233D+00,-.9840832904192385D+00, &
    0.8284898564271251D-01,-.8660720584668250D+00, &
    0.9809450037856990D+00,0.8048533499129369D+00, &
    0.2790289740151233D+00,-.4082743843038831D+00, &
    0.9740226654958868D+00,0.3035697582018394D+00, &
    -.9862725985120795D+00,0.7185672952170555D+00, &
    -.9193256145694509D+00,-.9544384245126734D+00, &
    -.9898146349839960D+00,0.7338947136259082D-01, &
    -.2075330463780984D+00,0.9907318998968393D+00, &
    0.5387986220469478D+00,0.8834131451429665D+00, &
    -.8934522638909749D+00,-.9207656972246965D+00, &
    -.6367076114754525D+00,0.9978913196317913D+00, &
    0.5300943949356414D+00,-.4262998259673880D+00, &
    0.1841144567160324D+00,0.5090691042712456D+00, &
    -.9214501612581178D+00,-.6400027811642439D+00, &
    0.3209231678914174D+00,-.3846636405176737D+00, &
    0.8697161095697501D+00,0.3470098428369617D+00, &
    0.5711239311935090D+00,0.4067912914297930D+00, &
    0.7624786524458222D+00,-.7770143745259210D+00, &
    0.9584356802550765D+00,0.6551973976459680D+00, &
    0.2854578772720695D-01,0.9551619780680761D+00, &
    -.1675185677029804D+00,0.4240021284329500D-01, &
    -.9864545039606490D+00,-.7989550278861819D+00, &
    -.6071839996966222D+00,0.7388672054082017D+00, &
    0.3140775120692750D+00,-.1793826353802805D+00, &
    0.8772336982049433D+00,-.6033076898494673D+00, &
    -.4039805189289538D+00,0.9282498625490752D+00, &
    0.7185098783764428D+00,0.9758090760986288D+00, &
    0.5388255455349226D+00,-.8020212431755418D+00, &
    0.8814335976449187D+00,0.7393267487868044D+00 /
   data ws / &
    0.4220810999582407D-01,0.4210468769855168D-01, &
    0.4111495137274749D-01,0.1602722818539338D-01, &
    0.3619967055440460D-01,0.5719761023753370D-01, &
    0.5553932136722374D-01,0.7027947643456225D-01, &
    0.3877224057962559D-01,0.5615678549912440D-01, &
    0.5631946977958934D-01,0.1879677326120675D-01, &
    0.1313057162810730D-02,0.2109221815204442D-01, &
    0.2451480331747192D-01,0.6556422061834499D-01, &
    0.1119235292343633D-01,0.7115563056913567D-02, &
    0.6714100241623257D-02,0.5885829448742507D-02, &
    0.8309083447844690D-01,0.3590868298232087D-01, &
    0.1246127664791048D-01,0.2018878910488524D-01, &
    0.8451815870413146D-01,0.3248692905689807D-01, &
    0.1341510929402505D-01,0.6472935261954060D-01, &
    0.1145237221800998D-01,0.5872712613961156D-01, &
    0.4508959666015173D-02,0.1493403238757605D-01, &
    0.2665017934074124D-02,0.7704806248110788D-01, &
    0.5726954359349042D-01,0.4528203295472384D-02, &
    0.4279817302411395D-01,0.4285481643886387D-01, &
    0.8072115177434423D-02,0.3701323444432086D-01, &
    0.4613328158120365D-01,0.3966194341947075D-02, &
    0.6788885180206457D-01,0.3480913280397030D-01, &
    0.1958947629336374D-01,0.7993874305725486D-01, &
    0.2433364291226502D-01,0.1062990044699051D-01, &
    0.6170560315322090D-01,0.7521981238448959D-01, &
    0.3621544028408890D-01,0.1839043728583574D-01, &
    0.3635800635820290D-01,0.1087561990443862D-01, &
    0.1235528041521356D-01,0.2670676504697193D-01, &
    0.5756245567725824D-02,0.1016410330068744D-01, &
    0.3155355506307831D-01,0.1843695052008212D-01, &
    0.1362846461516491D-01,0.8050841953401032D-01, &
    0.1413883594050338D-01,0.5019314017226575D-01, &
    0.5096315858878126D-01,0.4491888769330514D-01, &
    0.8622086129893808D-01,0.9032569802582428D-01, &
    0.4214427133348429D-01,0.1315668261444553D-01, &
    0.7308371765738744D-01,0.5122200740309473D-02, &
    0.6652468674436235D-01,0.1888655008314831D-01, &
    0.7163064878569746D-01,0.2352000357823148D-01, &
    0.2167515013056250D-01,0.4797944511124803D-01 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
  w(1:n) = ws(1:n)

  return
end
subroutine rule21 ( n, x, w )

!*****************************************************************************80
!
!! RULE21 returns the rule of degree 21.
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
  parameter ( ns = 82 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
    0.9754554632015029D+00,-.9754554632015029D+00, &
    0.9807628529972264D+00,-.9807628529972262D+00, &
    0.9864691810349792D+00,-.9864691810349792D+00, &
    0.9817752275809579D+00,-.9817752275809579D+00, &
    0.8942647503363536D+00,-.8942647503363533D+00, &
    -.7926169940479519D+00,0.7926169940479522D+00, &
    0.9673238098903247D+00,-.9673238098903247D+00, &
    -.6233424472175393D+00,0.6233424472175395D+00, &
    0.8075494298208362D+00,-.8075494298208362D+00, &
    -.9112339448388380D+00,0.9112339448388382D+00, &
    0.5294700780841287D+00,-.5294700780841285D+00, &
    0.9126054184220483D+00,-.9126054184220483D+00, &
    -.8005037324172778D+00,0.8005037324172778D+00, &
    -.9786316733709013D+00,0.9786316733709015D+00, &
    -.1931251782159229D+00,0.1931251782159230D+00, &
    0.7448428411561702D+00,-.7448428411561699D+00, &
    0.9910640341773119D+00,-.9910640341773119D+00, &
    0.4487876769216255D+00,-.4487876769216254D+00, &
    0.7730731265491211D+00,-.7730731265491211D+00, &
    -.8538350485874476D-02,0.8538350485874577D-02, &
    0.9305948052973293D+00,-.9305948052973293D+00, &
    -.4410686164106586D+00,0.4410686164106588D+00, &
    0.5726168020738448D+00,-.5726168020738448D+00, &
    0.9094921563319007D+00,-.9094921563319007D+00, &
    0.6650799110223121D+00,-.6650799110223121D+00, &
    0.9845379327215906D+00,-.9845379327215906D+00, &
    -.1489897109425285D-01,0.1489897109425291D-01, &
    0.4777858927462423D+00,-.4777858927462423D+00, &
    0.2346150739656185D-01,-.2346150739656185D-01, &
    -.6464750210052006D+00,0.6464750210052006D+00, &
    -.3898509033065152D+00,0.3898509033065153D+00, &
    -.2596817297614839D+00,0.2596817297614840D+00, &
    0.8930982971146899D-01,-.8930982971146888D-01, &
    0.2378419730518350D+00,-.2378419730518349D+00, &
    -.4923226235686091D+00,0.4923226235686091D+00, &
    0.3260553152624549D+00,-.3260553152624548D+00, &
    0.6900640280303905D+00,-.6900640280303905D+00, &
    0.8399949350854392D+00,-.8399949350854392D+00, &
    0.2175201355296100D+00,-.2175201355296099D+00, &
    -.2552655348509120D+00,0.2552655348509120D+00, &
    0.8967910069881992D+00,-.8967910069881992D+00 /
  data ys / &
    -.8684124523298049D+00,0.8684124523298051D+00, &
    -.9445108390325675D+00,0.9445108390325677D+00, &
    0.8224833612237958D+00,-.8224833612237956D+00, &
    -.6674381398519379D+00,0.6674381398519381D+00, &
    -.9903377411510502D+00,0.9903377411510502D+00, &
    -.9907543940322484D+00,0.9907543940322484D+00, &
    0.4473528589231669D-01,-.4473528589231656D-01, &
    -.9343866955469575D+00,0.9343866955469575D+00, &
    0.4520870573336636D+00,-.4520870573336634D+00, &
    -.9274277947725086D+00,0.9274277947725084D+00, &
    -.9866712872285782D+00,0.9866712872285782D+00, &
    -.4550564012095777D+00,0.4550564012095778D+00, &
    -.8127883212479330D+00,0.8127883212479330D+00, &
    -.9813307175904386D+00,0.9813307175904383D+00, &
    -.9279142876740704D+00,0.9279142876740704D+00, &
    -.9320431789499202D+00,0.9320431789499202D+00, &
    0.4282792320995888D+00,-.4282792320995887D+00, &
    -.5362236701446004D+00,0.5362236701446004D+00, &
    -.6399087385202717D+00,0.6399087385202717D+00, &
    -.8135057649888232D+00,0.8135057649888232D+00, &
    0.6470101042020234D+00,-.6470101042020232D+00, &
    -.8155924092874611D+00,0.8155924092874611D+00, &
    -.7941248493230142D+00,0.7941248493230142D+00, &
    0.2473440793805384D+00,-.2473440793805383D+00, &
    -.3454192145795777D+00,0.3454192145795778D+00, &
    -.2668503935770928D+00,0.2668503935770929D+00, &
    -.4625176827521818D+00,0.4625176827521818D+00, &
    -.6895677594851285D-01,0.6895677594851290D-01, &
    -.2404342948898286D-01,0.2404342948898286D-01, &
    -.6484663233565804D+00,0.6484663233565804D+00, &
    -.9898736812731077D+00,0.9898736812731077D+00, &
    -.6343263441026139D+00,0.6343263441026139D+00, &
    -.9814223809013130D+00,0.9814223809013130D+00, &
    -.2765567197102583D+00,0.2765567197102583D+00, &
    -.4080894058196405D+00,0.4080894058196404D+00, &
    -.9078967063955937D+00,0.9078967063955937D+00, &
    0.1497779073446914D+00,-.1497779073446913D+00, &
    -.1279062882959751D+00,0.1279062882959752D+00, &
    -.6902255847834162D+00,0.6902255847834162D+00, &
    -.2059314662901914D+00,0.2059314662901914D+00, &
    -.8238753864861620D+00,0.8238753864861620D+00 /
  data ws / &
    0.2118008413970087D-02,0.2118008413970087D-02, &
    0.4561991935236101D-02,0.4561991935236101D-02, &
    0.6876351365235698D-02,0.6876351365235698D-02, &
    0.1124466702102733D-01,0.1124466702102733D-01, &
    0.4818715192537238D-02,0.4818715192537238D-02, &
    0.5989564896092799D-02,0.5989564896092799D-02, &
    0.1767795981651036D-01,0.1767795981651036D-01, &
    0.2380089975465993D-01,0.2380089975465993D-01, &
    0.4148355130225962D-01,0.4148355130225962D-01, &
    0.1184358597738201D-01,0.1184358597738201D-01, &
    0.1089514188613057D-01,0.1089514188613057D-01, &
    0.3129460714416973D-01,0.3129460714416973D-01, &
    0.2951898806186879D-01,0.2951898806186879D-01, &
    0.2370685766690177D-02,0.2370685766690177D-02, &
    0.2822396585947564D-01,0.2822396585947564D-01, &
    0.2219335711268346D-01,0.2219335711268346D-01, &
    0.9583664348129897D-02,0.9583664348129897D-02, &
    0.6372673897970250D-01,0.6372673897970250D-01, &
    0.4375827379330900D-01,0.4375827379330900D-01, &
    0.4429543031127132D-01,0.4429543031127132D-01, &
    0.2471410210516637D-01,0.2471410210516637D-01, &
    0.4249825844225831D-01,0.4249825844225831D-01, &
    0.4558876680953739D-01,0.4558876680953739D-01, &
    0.2756490338260340D-01,0.2756490338260340D-01, &
    0.6243396099268773D-01,0.6243396099268773D-01, &
    0.1168182946833649D-01,0.1168182946833649D-01, &
    0.7382665975220684D-01,0.7382665975220684D-01, &
    0.7666679963909392D-01,0.7666679963909392D-01, &
    0.4391713647273730D-01,0.4391713647273730D-01, &
    0.4705013653249818D-01,0.4705013653249818D-01, &
    0.9574427334796009D-02,0.9574427334796009D-02, &
    0.6262807887841917D-01,0.6262807887841917D-01, &
    0.1507382504443397D-01,0.1507382504443397D-01, &
    0.7853972113541645D-01,0.7853972113541645D-01, &
    0.6890694586576196D-01,0.6890694586576196D-01, &
    0.3574201277291281D-01,0.3574201277291281D-01, &
    0.6408284590143187D-01,0.6408284590143187D-01, &
    0.4908916812527610D-01,0.4908916812527610D-01, &
    0.5502691558122864D-01,0.5502691558122864D-01, &
    0.8126618865329004D-01,0.8126618865329004D-01, &
    0.2206473054465979D-01,0.2206473054465979D-01 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
  w(1:n) = ws(1:n)

  return
end
subroutine rule22 ( n, x, w )

!*****************************************************************************80
!
!! RULE22 returns the rule of degree 22.
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
  parameter ( ns = 93 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
    -.1168773459913643D-01,-.7371881387645758D+00, &
    0.8294952934036814D+00,-.5954751704041336D+00, &
    -.4324369794733402D+00,-.7688482950823662D+00, &
    0.9204074310294709D+00,0.9345989584398445D-01, &
    -.8936155650763420D+00,-.7702459791051814D+00, &
    -.1373629144933700D+00,0.5509811434354096D+00, &
    -.1491245367450688D+00,0.3561825582103059D+00, &
    0.9353031268387029D+00,-.2924575431987692D+00, &
    0.2443965863661181D+00,-.6390847658587071D+00, &
    -.4984730279242101D+00,0.7392293471332223D+00, &
    -.9874262069353944D+00,-.9183316919543670D-01, &
    -.7612182335622527D+00,-.8617096626977842D-01, &
    0.9888069308129792D+00,-.5588987313328233D+00, &
    -.5751189401814027D+00,-.9225108518720977D+00, &
    0.8984615209750768D+00,0.8382620472105732D+00, &
    0.1626926025129410D+00,-.9967788148796793D+00, &
    -.6486037112886791D+00,-.5668454455159495D+00, &
    0.9856879280293260D+00,0.9717573267273887D+00, &
    -.3076008058514624D+00,0.3587016306051349D+00, &
    -.7623612106456181D+00,0.6388680957517785D+00, &
    0.5510537420201546D+00,0.1053488710493425D+00, &
    0.9916832391481870D+00,-.8977327473577641D+00, &
    0.4016301282586624D+00,-.9838441232429354D+00, &
    0.7905399896596540D+00,-.9824504337674188D+00, &
    -.5775436902276508D+00,-.9772275458992810D+00, &
    -.8927278831009643D+00,0.6072501413880697D+00, &
    0.3391264841215570D+00,0.8802736916527715D+00, &
    -.3614572700854332D+00,0.7628120974888042D+00, &
    0.9410464685954009D+00,0.5766667841609323D+00, &
    0.3785886954980848D+00,-.7336046871225165D+00, &
    -.3595240670259577D+00,0.1395498733753600D+00, &
    -.8379800610987231D+00,0.9026858439122091D+00, &
    -.7544128178700894D+00,-.1371254063139990D+00, &
    0.7326981395585584D+00,0.1008288194176049D+00, &
    -.1402954353279801D+00,0.5600957930752593D+00, &
    -.9835815408757524D+00,0.9794922442499654D+00, &
    -.3422476201633468D+00,0.8847947144492797D+00, &
    0.3480028949057162D+00,0.1399751797907734D+00, &
    0.7410269267329871D+00,-.8707213419949226D+00, &
    -.7829703967945006D+00,-.7518531217472861D-01, &
    0.1797607493737034D+00,0.5514938721272684D+00, &
    0.9189774855743247D+00,-.9478502721386363D+00, &
    -.3753442057328584D+00,0.9824594434447735D+00, &
    0.7794560599617187D+00,0.5868549502954560D+00, &
    -.9830132030284936D+00,-.9109869789024120D+00, &
    -.3369368343786870D+00,-.9656919342504539D+00, &
    0.9878086002165142D+00 /
  data ys / &
    0.2190948368619670D+00,-.5638633812668349D+00, &
    0.7772605528528743D+00,-.2566398183828403D+00, &
    -.5129047910078139D+00,-.4506194723257167D+00, &
    0.8425861006849568D+00,-.4518983245417754D+00, &
    0.3543337144777458D+00,0.4959761932603439D+00, &
    0.2485911682035064D+00,-.3132773446075675D+00, &
    -.9937282156984992D+00,-.2003997888710157D+00, &
    -.5607980017506478D+00,-.3522379616257777D+00, &
    -.4426431371959611D+00,-.3692415241854752D-01, &
    0.7496615691984209D+00,0.6926590385285363D+00, &
    -.1850687765410226D+00,-.1925693267588198D+00, &
    0.1363788459324560D+00,0.9515651662120697D+00, &
    0.5579343853339161D-01,0.9517652105508706D+00, &
    -.9389370283909926D+00,-.3771353916795439D+00, &
    -.9947202210423953D+00,-.3609118075153294D+00, &
    0.9902936741983037D+00,0.3133668776125615D+00, &
    0.6199365920821208D+00,0.2873358944700919D+00, &
    -.7434073625585887D+00,0.3666474918307532D+00, &
    0.8685050837394444D+00,0.9342984101597417D+00, &
    -.8496275328566278D+00,-.4918800631346358D+00, &
    0.5207829647449653D+00,0.4875959314271490D+00, &
    -.3759522691536638D+00,-.9452616179455003D+00, &
    -.6399489008638399D+00,-.5698761741539897D+00, &
    -.6821182736060648D+00,-.9838318115418717D+00, &
    -.7110797215818025D+00,-.8424531888684956D+00, &
    -.7019404243765398D+00,-.8133442851406898D+00, &
    0.6937136042433247D+00,0.1403628667969644D+00, &
    0.4853142503301350D+00,-.9404688641752270D+00, &
    -.1594253939471121D+00,0.8410558281897980D+00, &
    -.9090435768164216D+00,0.8618858293632953D+00, &
    -.9801201774367985D+00,0.2925291147308246D-01, &
    -.1389383474162093D+00,-.8488076014783108D+00, &
    -.9950422147374585D+00,0.6774278489603331D+00, &
    0.3281092020661301D+00,0.8336091093259060D+00, &
    -.6447342914933101D+00,-.9855619048067824D+00, &
    0.9863388067410731D+00,-.9474857272173681D+00, &
    -.8291238758195407D+00,0.5386555528488844D+00, &
    0.2895743833834052D+00,-.7848534988754099D+00, &
    -.1043902046106473D+00,0.7337865418484663D+00, &
    0.9901322140668845D+00,-.9181369104835962D+00, &
    -.9807391493006852D+00,0.7602577556121397D-01, &
    0.9902833594797992D+00,0.7639725935225382D-01, &
    0.1089930212643242D-01,0.6870529955030933D+00, &
    0.9401212847078045D+00,0.9891928053902059D+00, &
    0.8336461919417253D+00,0.9356252744608189D+00, &
    0.9934890766781104D+00,0.5866648232658751D+00, &
    0.9371299738758988D+00 /
  data ws / &
    0.6067121956035168D-02,0.2527104513859865D-01, &
    0.9025528955311097D-02,0.5803636803454140D-01, &
    0.4942739001800841D-01,0.2794072665175644D-01, &
    0.1603496923754518D-01,0.6013880064040703D-01, &
    0.2941264773858220D-01,0.3334592290253230D-01, &
    0.7897021060987033D-01,0.3685303544908184D-01, &
    0.3948104138816246D-02,0.6679001222692597D-01, &
    0.2111954975158539D-01,0.4792141258829016D-01, &
    0.2695976162319685D-01,0.2470945430647790D-01, &
    0.3947338905928970D-01,0.3294627756714954D-01, &
    0.9846011394782925D-02,0.7297001782497028D-01, &
    0.4106989048022942D-01,0.2161795857158166D-01, &
    0.8365413396512252D-02,0.1967362232030983D-01, &
    0.2188213465301344D-01,0.2494392526364261D-01, &
    0.3519731383753954D-02,0.3478325192787569D-01, &
    0.8190571090769309D-02,0.6108434551087310D-02, &
    0.3434968062753964D-01,0.6007998437323292D-01, &
    0.7504666111448662D-02,0.1603184108436172D-01, &
    0.3437516964723778D-01,0.2571624349884583D-01, &
    0.2694235332130491D-01,0.4466450734173113D-01, &
    0.5500707054662403D-01,0.7222938823304295D-01, &
    0.7486350326225666D-02,0.1143517520249437D-01, &
    0.5817171888321045D-01,0.9994978126228900D-02, &
    0.3467709360229788D-01,0.1964349392895841D-02, &
    0.4240321318174279D-01,0.8728495324106815D-02, &
    0.2591557111759762D-01,0.3720861411904331D-01, &
    0.5505688737445376D-01,0.3631603425342831D-01, &
    0.6567401226660792D-01,0.1795583567053723D-01, &
    0.2289367999547543D-01,0.3547488447682710D-01, &
    0.2995254108777568D-01,0.2686044680493277D-01, &
    0.1079041750322994D-01,0.7877736606773408D-01, &
    0.3733692480386966D-01,0.1821095142444074D-01, &
    0.5325368320113208D-02,0.5922160899036626D-01, &
    0.5076619476679856D-01,0.4372734278286988D-01, &
    0.6332661510009212D-01,0.9790273018996520D-02, &
    0.1776555836345017D-02,0.4813300061774173D-02, &
    0.4258882597057490D-01,0.3052111793035846D-01, &
    0.7003826037949971D-01,0.5120830680793959D-01, &
    0.4944440063240135D-01,0.2605683245528804D-01, &
    0.6201489406026509D-02,0.3089929178556832D-01, &
    0.1316878814412241D-01,0.6382065038385205D-01, &
    0.3990900163624796D-02,0.2334151205865433D-01, &
    0.7860422025755921D-01,0.1000548940130294D-01, &
    0.1798134856368640D-01,0.8411973937140507D-02, &
    0.7382035812019881D-02,0.1170576178999751D-01, &
    0.6920800809156403D-02,0.1595811031270932D-01, &
    0.3880611624295888D-02 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
  w(1:n) = ws(1:n)

  return
end
subroutine rule23 ( n, x, w )

!*****************************************************************************80
!
!! RULE23 returns the rule of degree 23.
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
  parameter ( ns = 98 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
    0.1922689542773817D+00,-.1922689542773816D+00, &
    -.9880860844879094D+00,0.9880860844879096D+00, &
    0.4588599108929147D+00,-.4588599108929146D+00, &
    0.2847661416086631D+00,-.2847661416086630D+00, &
    0.8023730521007825D+00,-.8023730521007822D+00, &
    -.4008300617025742D+00,0.4008300617025743D+00, &
    0.9128169992246806D+00,-.9128169992246804D+00, &
    0.6402796135160949D+00,-.6402796135160946D+00, &
    0.6316970739891037D+00,-.6316970739891037D+00, &
    -.5737921022421535D+00,0.5737921022421535D+00, &
    0.4346256934364524D+00,-.4346256934364522D+00, &
    0.7764676048715878D+00,-.7764676048715878D+00, &
    0.7244917270725689D+00,-.7244917270725689D+00, &
    -.1888739484686455D+00,0.1888739484686456D+00, &
    0.9943536134460622D+00,-.9943536134460622D+00, &
    -.7313948142734760D+00,0.7313948142734760D+00, &
    0.8943097577057932D+00,-.8943097577057932D+00, &
    0.8436479267198643D+00,-.8436479267198643D+00, &
    0.7791831851449014D+00,-.7791831851449014D+00, &
    0.8641774713700604D+00,-.8641774713700604D+00, &
    0.9617690298704565D+00,-.9617690298704563D+00, &
    -.4195385320317492D+00,0.4195385320317493D+00, &
    0.2226040785176633D+00,-.2226040785176632D+00, &
    0.9898168214058006D+00,-.9898168214058006D+00, &
    -.7489474270848241D+00,0.7489474270848243D+00, &
    0.1917103338448669D-01,-.1917103338448658D-01, &
    0.9493597561722253D+00,-.9493597561722253D+00, &
    0.9904394141829281D+00,-.9904394141829279D+00, &
    -.5829692270288439D+00,0.5829692270288439D+00, &
    -.6147395289066456D+00,0.6147395289066458D+00, &
    0.9535616627723794D+00,-.9535616627723796D+00, &
    -.2159492946526814D+00,0.2159492946526815D+00, &
    0.6262127585905694D+00,-.6262127585905694D+00, &
    0.9907200353310519D+00,-.9907200353310519D+00, &
    0.9843113976598121D+00,-.9843113976598121D+00, &
    -.8786718625785459D+00,0.8786718625785461D+00, &
    0.9324996820585237D+00,-.9324996820585237D+00, &
    0.8556365285580105D+00,-.8556365285580105D+00, &
    0.6098296498831587D+00,-.6098296498831587D+00, &
    0.2401978383668566D-02,-.2401978383668536D-02, &
    -.2160362298752296D+00,0.2160362298752297D+00, &
    0.9419047852611938D+00,-.9419047852611938D+00, &
    0.9784325270256451D+00,-.9784325270256451D+00, &
    0.4245835794119392D+00,-.4245835794119392D+00, &
    0.2180806198402934D+00,-.2180806198402933D+00, &
    0.4211715480246619D+00,-.4211715480246619D+00, &
    0.2159184187190165D+00,-.2159184187190165D+00, &
    0.9145012870119151D+00,-.9145012870119151D+00, &
    -.6659715296954374D-02,0.6659715296954456D-02 /
  data ys / &
    -.8413204499428454D+00,0.8413204499428454D+00, &
    -.9814588472800125D+00,0.9814588472800123D+00, &
    -.6684346870698402D+00,0.6684346870698402D+00, &
    -.7992729842209235D+00,0.7992729842209235D+00, &
    -.9416742417176776D+00,0.9416742417176776D+00, &
    -.6678440121187212D+00,0.6678440121187212D+00, &
    -.9891421484914774D+00,0.9891421484914776D+00, &
    -.9915720334011255D+00,0.9915720334011255D+00, &
    -.4898989236348730D+00,0.4898989236348731D+00, &
    -.4662751769077947D+00,0.4662751769077946D+00, &
    -.9416451658784137D+00,0.9416451658784137D+00, &
    -.2882717499568009D+00,0.2882717499568010D+00, &
    0.2486649083667506D+00,-.2486649083667505D+00, &
    -.9882692965051619D+00,0.9882692965051619D+00, &
    0.7885280263196212D+00,-.7885280263196209D+00, &
    -.6589298745979582D+00,0.6589298745979582D+00, &
    -.5251907426680982D+00,0.5251907426680982D+00, &
    0.2698952252390472D-01,-.2698952252390462D-01, &
    -.7058148204376444D+00,0.7058148204376444D+00, &
    0.8039684450793269D+00,-.8039684450793269D+00, &
    -.9795669903911515D+00,0.9795669903911517D+00, &
    -.9436139885736695D+00,0.9436139885736695D+00, &
    -.9897986315498021D+00,0.9897986315498021D+00, &
    -.1515697280553813D-02,0.1515697280553934D-02, &
    -.9288927030420643D+00,0.9288927030420643D+00, &
    -.9319407769628315D+00,0.9319407769628315D+00, &
    0.6363710989431733D+00,-.6363710989431731D+00, &
    -.9179467622904829D+00,0.9179467622904831D+00, &
    -.8263113277740309D+00,0.8263113277740309D+00, &
    -.9928811295871781D+00,0.9928811295871781D+00, &
    0.9091589955140115D+00,-.9091589955140112D+00, &
    -.4548720285260120D+00,0.4548720285260120D+00, &
    -.8475542857772346D+00,0.8475542857772346D+00, &
    0.4247422802343108D+00,-.4247422802343107D+00, &
    -.3947150748420805D+00,0.3947150748420806D+00, &
    -.9832744227413978D+00,0.9832744227413978D+00, &
    -.1923063082978693D+00,0.1923063082978694D+00, &
    0.4580266855460115D+00,-.4580266855460114D+00, &
    -.4698541060861779D-01,0.4698541060861786D-01, &
    -.2437626817883838D+00,0.2437626817883838D+00, &
    -.8348003321715464D+00,0.8348003321715464D+00, &
    0.2268917425003757D+00,-.2268917425003756D+00, &
    -.6927513253690014D+00,0.6927513253690016D+00, &
    -.2736597506928621D+00,0.2736597506928621D+00, &
    -.4885665960318681D+00,0.4885665960318681D+00, &
    0.2093478053463650D+00,-.2093478053463650D+00, &
    -.1584479979856305D-01,0.1584479979856307D-01, &
    -.8398490539050634D+00,0.8398490539050636D+00, &
    -.6725993072235357D+00,0.6725993072235357D+00 /
  data ws / &
    0.2087996398690324D-01,0.2087996398690324D-01, &
    0.1454948755827531D-02,0.1454948755827531D-02, &
    0.4748321475897626D-01,0.4748321475897626D-01, &
    0.2581005409389433D-01,0.2581005409389433D-01, &
    0.1501004260111713D-01,0.1501004260111713D-01, &
    0.4842495146008856D-01,0.4842495146008856D-01, &
    0.2506826413864942D-02,0.2506826413864942D-02, &
    0.6535276655159778D-02,0.6535276655159778D-02, &
    0.4894280385061923D-01,0.4894280385061923D-01, &
    0.4975905503606815D-01,0.4975905503606815D-01, &
    0.2214413184536724D-01,0.2214413184536724D-01, &
    0.4313124475180520D-01,0.4313124475180520D-01, &
    0.4433897643935200D-01,0.4433897643935200D-01, &
    0.9046276329783944D-02,0.9046276329783944D-02, &
    0.3911096929389304D-02,0.3911096929389304D-02, &
    0.3599231426419872D-01,0.3599231426419872D-01, &
    0.2838276711743061D-01,0.2838276711743061D-01, &
    0.3373992192798946D-01,0.3373992192798946D-01, &
    0.3336454855497337D-01,0.3336454855497337D-01, &
    0.2095059245259030D-01,0.2095059245259030D-01, &
    0.2046044703097752D-02,0.2046044703097752D-02, &
    0.2164205919445887D-01,0.2164205919445887D-01, &
    0.8285628342573098D-02,0.8285628342573098D-02, &
    0.8397251850551729D-02,0.8397251850551729D-02, &
    0.1731140501927820D-01,0.1731140501927820D-01, &
    0.2467601783099452D-01,0.2467601783099452D-01, &
    0.1682157937509966D-01,0.1682157937509966D-01, &
    0.3331814718792869D-02,0.3331814718792869D-02, &
    0.3259337068821334D-01,0.3259337068821334D-01, &
    0.6005493842446140D-02,0.6005493842446140D-02, &
    0.8210801440654199D-02,0.8210801440654199D-02, &
    0.6548837153964436D-01,0.6548837153964436D-01, &
    0.3088139176161778D-01,0.3088139176161778D-01, &
    0.7183392776926659D-02,0.7183392776926659D-02, &
    0.9748230121097153D-02,0.9748230121097153D-02, &
    0.5383514720217621D-02,0.5383514720217621D-02, &
    0.2224440941283463D-01,0.2224440941283463D-01, &
    0.3213763319312607D-01,0.3213763319312607D-01, &
    0.5959500864875663D-01,0.5959500864875663D-01, &
    0.7353356562572214D-01,0.7353356562572214D-01, &
    0.3920268743920687D-01,0.3920268743920687D-01, &
    0.2191097090277860D-01,0.2191097090277860D-01, &
    0.1035031173873475D-01,0.1035031173873475D-01, &
    0.6659548623039759D-01,0.6659548623039759D-01, &
    0.6553251667930458D-01,0.6553251667930458D-01, &
    0.6644084983327379D-01,0.6644084983327379D-01, &
    0.7384544963870446D-01,0.7384544963870446D-01, &
    0.1649581640678158D-01,0.1649581640678158D-01, &
    0.5651348047241081D-01,0.5651348047241081D-01 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
  w(1:n) = ws(1:n)

  return
end
subroutine rule24 ( n, x, w )

!*****************************************************************************80
!
!! RULE24 returns the rule of degree 24.
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
  parameter ( ns = 109 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
    -.9947148928250171D+00,0.9910990446809216D+00, &
    0.9693866534373358D+00,0.8366669887301708D+00, &
    0.9976180969133432D+00,-.9626395872771291D+00, &
    -.9956012694874316D+00,0.9827883755011416D+00, &
    0.9899696481565038D+00,-.1027795858264751D+00, &
    -.5372013809570906D+00,-.1302736589054738D+00, &
    -.9966597496356319D+00,-.8416889461091814D+00, &
    -.9888026187105032D+00,0.5314747340473951D+00, &
    -.8162827902273060D+00,0.9739798586001176D+00, &
    -.9411304883795972D+00,-.9821743630414852D+00, &
    -.8146483736791913D+00,0.2216111751930999D+00, &
    0.9919273511735743D+00,0.8515268909036164D+00, &
    -.6344208939912110D-01,0.8124697219833941D+00, &
    -.9159342331978603D+00,0.1939576002609792D+00, &
    0.9879450437516667D+00,0.2535126040886103D+00, &
    -.9628662071921108D+00,0.8784885445188073D+00, &
    -.8980693800663294D+00,-.4876187319409098D+00, &
    0.8244493216582833D+00,-.7771226957924775D-01, &
    0.9133806640305893D+00,-.2863497657037777D+00, &
    -.7746256411943806D+00,0.8788745310361548D+00, &
    -.9610911899226917D+00,0.9600320888811048D+00, &
    0.9425138210931967D+00,0.7626320556994604D+00, &
    -.3308772778351785D+00,0.1279919952613696D+00, &
    0.5294732769614892D+00,0.3555627902477361D+00, &
    -.9279977222115954D+00,0.9931449449560651D+00, &
    0.5102490315575532D-01,-.7525118688957001D+00, &
    -.4719724839934526D+00,0.1520260254758981D+00, &
    0.9489604126336816D+00,-.2169062988895815D+00, &
    -.8687157102597637D+00,-.7360447596642431D+00, &
    0.8497144016937899D+00,-.6403949304059380D+00, &
    0.9160879280172893D+00,0.5974122417304695D+00, &
    0.9356564573866695D+00,0.2602110178808541D-02, &
    0.2185370159809754D+00,-.4174601636720920D+00, &
    -.6704915695516799D+00,0.7098684394657311D+00, &
    -.6662903518807954D+00,0.6842750704411169D+00, &
    0.6940401495592319D+00,0.2055436622280245D+00, &
    0.5409016726579078D+00,-.2111056222157408D+00, &
    0.4249715565953610D+00,-.4579852398972861D+00, &
    -.4762834069005017D+00,-.3112968492969387D+00, &
    -.9861402159543649D+00,0.5994434867749180D+00, &
    -.6329536320758256D+00,0.4071516862564607D+00, &
    -.5899049368040069D+00,-.8873886456704851D+00, &
    -.9429884035167735D+00,-.8103411028772554D+00, &
    -.6172197178932676D+00,0.4403002993242678D+00, &
    -.5964555068203565D+00,0.4214363261995724D+00, &
    -.2167017340827659D+00,0.3600751655325609D+00, &
    0.9573652308066442D-02,-.1986519498186896D+00, &
    0.5940360363603530D+00,0.7449094683537840D+00, &
    0.3446198859685110D+00,0.7550496004921502D+00, &
    0.2034447821293972D+00,-.8764429892755836D+00, &
    -.9821148921468710D-02,-.4283906719131795D+00, &
    -.2565291139819166D+00,-.4253124084387095D+00, &
    -.4107973035474266D+00,0.6107899647074595D+00, &
    -.7740269973316680D+00,-.9874686104628365D+00, &
    -.1780069523272345D-01 /
  data ys / &
    0.9921816068213789D+00,0.9927512676582938D+00, &
    -.9807210913639813D+00,-.9849335979706825D+00, &
    -.9031203174435843D+00,-.9807185407289059D+00, &
    0.1826280218638680D+00,0.1985977578306220D-03, &
    -.1343242025779884D+00,0.9836572598301550D+00, &
    -.9945228775385402D+00,-.9932466930618153D+00, &
    -.9021729201562282D+00,0.9919450680865269D+00, &
    -.5469411165135486D+00,-.9946152107188805D+00, &
    -.9833999582719447D+00,-.7694911408818028D+00, &
    0.9454206624475765D+00,0.5820485751363867D+00, &
    0.8726697552774600D+00,0.9884035519660979D+00, &
    0.3773859079404019D+00,0.9854732000616159D+00, &
    0.7430686212004588D+00,-.8172733392382462D+00, &
    0.7368090773498677D+00,0.8191066542200217D+00, &
    -.5129857572745989D+00,0.7339438859190308D+00, &
    0.3121292180404563D+00,0.4238556817241846D+00, &
    -.9023577502860924D+00,-.8635258163522489D+00, &
    -.5019428343401774D+00,-.9100031022978671D+00, &
    -.6538056836085887D+00,0.9419753943798090D+00, &
    -.8083978001656728D+00,0.7971819583886433D+00, &
    -.7612052337333153D+00,0.6275466264499783D+00, &
    0.1573227985990733D+00,0.6249214522371203D+00, &
    -.9587696584061124D+00,-.9822260622287698D+00, &
    -.8459845466706856D+00,-.6962190414609735D+00, &
    -.3797057196057647D+00,0.8208860694583049D+00, &
    0.9117911472030502D+00,0.2466478148353290D+00, &
    0.2531249337761478D+00,-.8311014632352622D+00, &
    0.9327963019854183D+00,0.3901208978319467D+00, &
    -.6258430618900800D+00,-.4854699629184363D+00, &
    -.8013355833035751D-01,0.7663811452715913D+00, &
    -.9064248594697549D+00,0.9816002629218432D+00, &
    -.3156827868701995D+00,0.5928915583696132D+00, &
    -.2758048916460264D-01,0.6590354293018823D+00, &
    -.9381955060600012D+00,-.2998249948498765D+00, &
    0.9581605160533719D+00,-.6881340269166623D+00, &
    -.9406665893940601D+00,0.3996485712287530D+00, &
    -.5101678225103823D+00,-.4421541038328301D+00, &
    0.5912997929743822D+00,0.9932623278482910D+00, &
    0.8781241163707275D+00,0.5625436230792860D+00, &
    -.1854744570391582D+00,0.7725191379684401D+00, &
    -.2272561659397919D-01,-.2598606537214366D+00, &
    0.4549553364199058D+00,0.4412559753061466D+00, &
    -.7387531735800691D-02,-.1993131251585695D+00, &
    -.7001646090032370D+00,0.8894305723468252D+00, &
    -.3651421534696686D+00,0.2021031040499760D+00, &
    -.4759573989947036D-01,0.9560701833067996D+00, &
    -.2523062776553245D+00,0.8029027500701331D+00, &
    -.3052326412673496D-01,0.9085765488533346D+00, &
    -.9429472498836997D+00,0.1985140340578119D+00, &
    -.4744209684134443D+00,0.8925996493449884D-01, &
    0.1788338183287970D+00,-.5864723938922081D+00, &
    -.7750597333889675D+00,-.2335671290163587D+00, &
    0.1465570094281399D+00,0.4191985939494091D+00, &
    0.6100181397154107D+00,0.8429972982275405D+00, &
    -.6496814943785820D+00 /
  data ws / &
    0.7673500038857188D-03,0.8868846753542908D-03, &
    0.2702370475569613D-02,0.5392375480513576D-02, &
    0.1761998115149276D-02,0.3252629117203137D-02, &
    0.4483878506691741D-02,0.4387445903456329D-02, &
    0.6293360138903417D-02,0.9110559046011608D-02, &
    0.4960172913990594D-02,0.4540132762921259D-02, &
    0.2380910242803054D-02,0.4079407684566781D-02, &
    0.7372795522491610D-02,0.5764065832069020D-02, &
    0.5996283579933687D-02,0.8231405547532079D-02, &
    0.7742564602285441D-02,0.9689437679198899D-02, &
    0.1992786357523802D-01,0.7578960870331433D-02, &
    0.7297515243700454D-02,0.5560565572125132D-02, &
    0.1431910744302743D-01,0.2083181247879573D-01, &
    0.1871125538452946D-01,0.1856480159069449D-01, &
    0.7926961712192319D-02,0.3149563402986980D-01, &
    0.1220979773721510D-01,0.3064077139403503D-01, &
    0.1246839879952511D-01,0.2727439670515271D-01, &
    0.2989955062354429D-01,0.2568927037462639D-01, &
    0.1731775418487197D-01,0.1651762549497732D-01, &
    0.2348412516559215D-01,0.2103194138017257D-01, &
    0.1218315588129536D-01,0.1587611356755889D-01, &
    0.2192984739792703D-01,0.3559892184688495D-01, &
    0.1598768389647512D-01,0.9998012603521544D-02, &
    0.3014828623151700D-01,0.4445807626369159D-01, &
    0.2303305529498344D-01,0.4177457820842124D-02, &
    0.2595692066159871D-01,0.4297075322654262D-01, &
    0.2734909126669048D-01,0.3576989995633175D-01, &
    0.8104038598451695D-02,0.5896633070443507D-01, &
    0.2530733931631170D-01,0.3397799558910337D-01, &
    0.3681905878262371D-01,0.3127418749936042D-01, &
    0.1043340150186936D-01,0.9877922117697804D-02, &
    0.2268684828187055D-01,0.5274200508727377D-01, &
    0.6668503169982437D-01,0.4053161003772505D-01, &
    0.1580741394786280D-01,0.4437844031312334D-01, &
    0.1465934495793213D-01,0.3316127050631540D-01, &
    0.1633165248572775D-01,0.6234782740688132D-01, &
    0.4701994260472551D-01,0.5761066620189915D-01, &
    0.4794170534889713D-01,0.5798541312289998D-02, &
    0.2505250386803961D-01,0.2055053414266298D-01, &
    0.8995847445576632D-02,0.3472177169167036D-01, &
    0.5444084929958415D-01,0.5971645153263631D-01, &
    0.4912587830256034D-01,0.2538968252075221D-01, &
    0.1542318408388366D-01,0.4013335870948910D-01, &
    0.3298688496369413D-01,0.2322125010689848D-01, &
    0.3671677541506277D-01,0.6139982772160751D-01, &
    0.6362592843510947D-01,0.9567538562556033D-02, &
    0.6459890039693177D-01,0.3562315409338049D-01, &
    0.5516254605923790D-01,0.1998312905915403D-01, &
    0.2052118732484228D-01,0.4458997988822495D-01, &
    0.5811519536921120D-01,0.2297223481430297D-01, &
    0.6766667516677112D-01,0.4532069980219111D-01, &
    0.4089559205492352D-01,0.5328231011100253D-01, &
    0.4369211916320918D-01,0.4895075796108013D-01, &
    0.3261352596485557D-01,0.5100190068919709D-02, &
    0.5182667283735970D-01 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
  w(1:n) = ws(1:n)

  return
end
subroutine rule25 ( n, x, w )

!*****************************************************************************80
!
!! RULE25 returns the rule of degree 25.
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
  parameter ( ns = 115 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
    -.7103850079486462D+00,0.7103850079486462D+00, &
    0.2350136833954603D+00,-.2350136833954603D+00, &
    0.2319467644431913D+00,-.2319467644431913D+00, &
    0.1887551793322332D-01,-.1887551793322329D-01, &
    0.4762227841581165D+00,-.4762227841581164D+00, &
    0.9223578267424110D+00,-.9223578267424110D+00, &
    0.8612991449140567D+00,-.8612991449140567D+00, &
    -.2008507367686636D+00,0.2008507367686637D+00, &
    0.9910315414864325D+00,-.9910315414864325D+00, &
    0.3217353529776810D+00,-.3217353529776809D+00, &
    0.8933439162316077D+00,-.8933439162316077D+00, &
    -.5783268726892828D+00,0.5783268726892828D+00, &
    0.1876981715173633D+00,-.1876981715173632D+00, &
    -.4040937317707446D+00,0.4040937317707447D+00, &
    -.8086692861933220D+00,0.8086692861933220D+00, &
    0.7699071344451243D+00,-.7699071344451243D+00, &
    0.1349449671364110D+00,-.1349449671364109D+00, &
    0.7067483874126944D+00,-.7067483874126944D+00, &
    0.9941851173182269D+00,-.9941851173182271D+00, &
    0.9823116186120971D+00,-.9823116186120969D+00, &
    0.4238538814747814D-01,-.4238538814747803D-01, &
    -.1196214241956023D+00,0.1196214241956024D+00, &
    0.6534777415625678D+00,-.6534777415625675D+00, &
    0.9750429947957491D+00,-.9750429947957491D+00, &
    0.8117686312525225D+00,-.8117686312525225D+00, &
    0.6171167577694838D+00,-.6171167577694838D+00, &
    0.5721736374948944D+00,-.5721736374948944D+00, &
    -.3415910753286391D+00,0.3415910753286392D+00, &
    -.9491077472873352D+00,0.9491077472873354D+00, &
    0.9925898844290307D+00,-.9925898844290307D+00, &
    -.2361386817708837D+00,0.2361386817708838D+00, &
    0.4376492710177434D+00,-.4376492710177433D+00, &
    0.9907335874081320D+00,-.9907335874081320D+00, &
    0.3690348754186010D+00,-.3690348754186009D+00, &
    0.2356776284326877D+00,-.2356776284326877D+00, &
    0.9290327147765426D+00,-.9290327147765426D+00, &
    0.5315931005621322D+00,-.5315931005621322D+00, &
    -.7970927883912756D+00,0.7970927883912758D+00, &
    -.6382950957253196D+00,0.6382950957253198D+00, &
    0.9125245471252791D+00,-.9125245471252793D+00, &
    -.1417325966030156D+00,0.1417325966030157D+00, &
    0.4628221381215424D+00,-.4628221381215424D+00, &
    0.9500761399436652D+00,-.9500761399436652D+00, &
    0.6585082411431559D+00,-.6585082411431559D+00, &
    -.5874365186841102D-02,0.5874365186841170D-02, &
    0.9830805981225823D+00,-.9830805981225823D+00, &
    0.9607858073894220D+00,-.9607858073894220D+00, &
    0.8126469869288578D+00,-.8126469869288578D+00, &
    0.9197969485441327D+00,-.9197969485441327D+00, &
    -.5479961396095706D+00,0.5479961396095708D+00, &
    -.4470049599321014D+00,0.4470049599321015D+00, &
    0.7873844821702739D+00,-.7873844821702737D+00, &
    0.3012570662899303D-30,0.4368201344745598D+00, &
    -.4368201344745598D+00,0.8506134670179059D+00, &
    -.8506134670179059D+00,0.9113188530371741D+00, &
    -.9113188530371739D+00,0.9836230570941239D+00, &
    -.9836230570941239D+00,0.7402837708508578D+00, &
    -.7402837708508578D+00 /
  data ys / &
    -.7506170245096466D+00,0.7506170245096466D+00, &
    -.6015670998122035D-01,0.6015670998122038D-01, &
    0.6313639161925437D-01,-.6313639161925434D-01, &
    -.2037217515148590D+00,0.2037217515148590D+00, &
    -.9891004866173655D+00,0.9891004866173655D+00, &
    -.3246552383950583D+00,0.3246552383950584D+00, &
    -.1540352101742460D+00,0.1540352101742461D+00, &
    -.3377511016941964D+00,0.3377511016941964D+00, &
    -.6383724442924701D+00,0.6383724442924703D+00, &
    -.8954389923107672D+00,0.8954389923107672D+00, &
    0.6633371340392643D+00,-.6633371340392643D+00, &
    -.6441606686218248D+00,0.6441606686218248D+00, &
    -.9825525591494771D+00,0.9825525591494771D+00, &
    -.4921744377774439D+00,0.4921744377774439D+00, &
    -.8285906504912744D+00,0.8285906504912744D+00, &
    0.5016460262480213D+00,-.5016460262480213D+00, &
    -.7648310056858745D+00,0.7648310056858745D+00, &
    -.7008712571932789D+00,0.7008712571932789D+00, &
    0.9429421827689296D+00,-.9429421827689294D+00, &
    -.9889464727951927D+00,0.9889464727951929D+00, &
    -.9395791578550448D+00,0.9395791578550448D+00, &
    -.8631057904957280D+00,0.8631057904957280D+00, &
    -.9411634392706579D+00,0.9411634392706579D+00, &
    0.7901512634507242D+00,-.7901512634507240D+00, &
    -.8620663504329279D+00,0.8620663504329279D+00, &
    0.3181861315943668D+00,-.3181861315943667D+00, &
    -.4998028335682857D+00,0.4998028335682858D+00, &
    -.9494727492703661D+00,0.9494727492703661D+00, &
    -.9900142583759359D+00,0.9900142583759357D+00, &
    -.2005292720209485D+00,0.2005292720209486D+00, &
    -.6936010697328517D+00,0.6936010697328517D+00, &
    -.9618522647938675D+00,0.9618522647938675D+00, &
    0.2336705239348026D+00,-.2336705239348024D+00, &
    -.6422765133729448D+00,0.6422765133729448D+00, &
    -.4058616631814986D+00,0.4058616631814986D+00, &
    -.7553494223525696D+00,0.7553494223525699D+00, &
    -.8131056508171524D+00,0.8131056508171524D+00, &
    -.9753835609823820D+00,0.9753835609823820D+00, &
    -.9152847871689571D+00,0.9152847871689571D+00, &
    0.9075396692621631D+00,-.9075396692621629D+00, &
    -.9916640269515081D+00,0.9916640269515081D+00, &
    -.2319599091440556D+00,0.2319599091440556D+00, &
    0.2744706063362164D-01,-.2744706063362152D-01, &
    -.4230837956698025D-01,0.4230837956698034D-01, &
    -.5553447391265243D+00,0.5553447391265243D+00, &
    0.5512083775806552D+00,-.5512083775806550D+00, &
    -.4486213975897800D+00,0.4486213975897801D+00, &
    0.1653383270128847D+00,-.1653383270128846D+00, &
    0.3695711955638312D+00,-.3695711955638311D+00, &
    -.9920191871388367D+00,0.9920191871388367D+00, &
    -.8211053506315364D+00,0.8211053506315364D+00, &
    -.9936569020855669D+00,0.9936569020855669D+00, &
    -.1899153506582438D-30,0.1367147397139465D+00, &
    -.1367147397139464D+00,-.5673411356087702D+00, &
    0.5673411356087702D+00,-.9545649683609251D+00, &
    0.9545649683609253D+00,-.8813580692740732D+00, &
    0.8813580692740735D+00,-.3448010185338413D+00, &
    0.3448010185338414D+00 /
  data ws / &
    0.1890289035282082D-01,0.1890289035282082D-01, &
    0.4408519139496046D-01,0.4408519139496046D-01, &
    0.1760012528851595D-01,0.1760012528851595D-01, &
    0.5135792991631447D-01,0.5135792991631447D-01, &
    0.5903718396467129D-02,0.5903718396467129D-02, &
    0.1169110569868906D-01,0.1169110569868906D-01, &
    0.2797734321676553D-01,0.2797734321676553D-01, &
    0.5430655753317624D-01,0.5430655753317624D-01, &
    0.5579615408482969D-02,0.5579615408482969D-02, &
    0.2529071368793077D-01,0.2529071368793077D-01, &
    0.2144063823465160D-01,0.2144063823465160D-01, &
    0.3501744210151039D-01,0.3501744210151039D-01, &
    0.8405006765794200D-02,0.8405006765794200D-02, &
    0.4833682312227475D-01,0.4833682312227475D-01, &
    0.1594545160823835D-01,0.1594545160823835D-01, &
    0.3558939192260235D-01,0.3558939192260235D-01, &
    0.4226984758231494D-01,0.4226984758231494D-01, &
    0.3140501945419725D-01,0.3140501945419725D-01, &
    0.2024434452721722D-02,0.2024434452721722D-02, &
    0.1349622146875648D-02,0.1349622146875648D-02, &
    0.1534182110464278D-01,0.1534182110464278D-01, &
    0.2952379758820060D-01,0.2952379758820060D-01, &
    0.1647236890585741D-01,0.1647236890585741D-01, &
    0.8590118628187588D-02,0.8590118628187588D-02, &
    0.1949659427579714D-01,0.1949659427579714D-01, &
    0.4728589565124858D-01,0.4728589565124858D-01, &
    0.4410722929300222D-01,0.4410722929300222D-01, &
    0.1872860198784019D-01,0.1872860198784019D-01, &
    0.2411335871046539D-02,0.2411335871046539D-02, &
    0.6741232108638781D-02,0.6741232108638781D-02, &
    0.4411511228498587D-01,0.4411511228498587D-01, &
    0.4232733780262361D-02,0.4232733780262361D-02, &
    0.6812146781301679D-02,0.6812146781301679D-02, &
    0.4593608235278822D-01,0.4593608235278822D-01, &
    0.5792082516805559D-01,0.5792082516805559D-01, &
    0.1593195877796943D-01,0.1593195877796943D-01, &
    0.3056610289854859D-01,0.3056610289854859D-01, &
    0.8554835231990672D-02,0.8554835231990672D-02, &
    0.1956601012663267D-01,0.1956601012663267D-01, &
    0.1051117872480982D-01,0.1051117872480982D-01, &
    0.6521513110348300D-02,0.6521513110348300D-02, &
    0.5717074378851367D-01,0.5717074378851367D-01, &
    0.1929092010649333D-01,0.1929092010649333D-01, &
    0.5062498553433200D-01,0.5062498553433200D-01, &
    0.5402053691556560D-01,0.5402053691556560D-01, &
    0.8931127348310931D-02,0.8931127348310931D-02, &
    0.1271750255435475D-01,0.1271750255435475D-01, &
    0.3860453155876481D-01,0.3860453155876481D-01, &
    0.2372095848832025D-01,0.2372095848832025D-01, &
    0.5971489271210207D-02,0.5971489271210207D-02, &
    0.3192818361403054D-01,0.3192818361403054D-01, &
    0.4118255231945603D-02,0.4118255231945603D-02, &
    0.1843978737274201D-01,0.5393241195495463D-01, &
    0.5393241195495463D-01,0.2787139422709562D-01, &
    0.2787139422709562D-01,0.8002087241718343D-02, &
    0.8002087241718343D-02,0.5159834028120074D-02, &
    0.5159834028120074D-02,0.3908234388553434D-01, &
    0.3908234388553434D-01 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
  w(1:n) = ws(1:n)

  return
end
subroutine rule26 ( n, x, w )

!*****************************************************************************80
!
!! RULE26 returns the rule of degree 26.
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
  parameter ( ns = 127 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs  / &
    0.4833333960601769D+00,0.8342454172040494D+00, &
    0.7576455307285274D+00,-.5897785388721970D+00, &
    -.1965092140379674D+00,-.7806410759767570D+00, &
    0.7735458124538029D+00,0.2194354253840281D+00, &
    -.6123258347540325D+00,-.3875195310855452D+00, &
    0.4210764575469916D+00,0.6717143218134665D+00, &
    0.4937567886903959D+00,0.4310467214948753D+00, &
    -.8221934052978648D-01,0.1474744561277340D+00, &
    -.7849971862231101D+00,0.2788743412255506D+00, &
    -.9174091785830387D+00,-.2680062461938560D+00, &
    -.6869484833848130D+00,-.5769800391396182D+00, &
    -.3175999245771031D+00,-.6900722826105213D+00, &
    -.4494495234589641D+00,0.2720327015362536D+00, &
    0.9844767989506050D+00,0.9539566427635388D+00, &
    -.6618689211018638D+00,0.9948047635608559D+00, &
    0.6791817577488417D+00,-.7660679944748241D+00, &
    -.3544317327177745D+00,0.8846710719994197D+00, &
    -.4769291205740912D+00,0.3374006730427574D+00, &
    -.5230046201031646D+00,0.9773104137186888D+00, &
    0.8460741659581372D+00,0.7811230642538761D+00, &
    -.7917954564682173D+00,0.6689548567706126D+00, &
    0.8995533103742481D+00,0.5803086536096742D+00, &
    0.9125294970192420D+00,0.9864324103956210D+00, &
    -.9634751680719612D+00,0.9171821405508755D+00, &
    -.8464504414046380D+00,0.1148682640972169D-01, &
    -.8893878433260284D+00,0.1985037063041746D+00, &
    0.6321992170210116D+00,-.8764667800594454D-01, &
    0.9612042270501360D+00,0.8449547678280973D+00, &
    -.3143374729428800D-01,-.7221561416927453D+00, &
    -.7408359655977623D+00,-.6573270993518512D+00, &
    0.7579600253683801D+00,0.1336947783582028D+00, &
    -.8801486144575378D+00,0.9863146730096674D+00, &
    0.5018023174101623D+00,0.5456331728950238D+00, &
    0.9966649790149599D+00,-.3664122751853194D-01, &
    0.3253875792212299D-01,0.9510103999741907D+00, &
    0.4259089402020555D+00,0.5239001877754490D+00, &
    0.6028877215019741D+00,0.3449303649336414D+00, &
    -.5679703819864291D+00,-.5650724066199918D+00, &
    -.8419709514596180D-01,0.8402671970302827D-01, &
    -.9108885214110795D+00,0.8192687649356658D+00, &
    -.9543907466564233D+00,-.7146063535728425D+00, &
    -.3837061453087470D+00,0.4746594317998059D-01, &
    -.1741432512718204D+00,0.2800861092084038D+00, &
    -.8207343144238776D+00,-.9935648502810311D+00, &
    -.8697847685325190D+00,-.2948353200874713D+00, &
    -.8584906175082293D+00,0.8306678364449214D+00, &
    -.9930049118920397D+00,-.4621304609915161D+00, &
    0.1460010936826870D+00,-.9839106590889559D+00, &
    -.4906721644588728D+00,-.9896275589251841D+00, &
    -.9930899945869894D+00,0.9038157695421652D-01, &
    0.9976519966165041D+00,0.9439357544987950D+00, &
    -.8915811088898837D+00,0.9306112013193892D+00, &
    -.9907255740995738D+00,-.9338876300481137D+00, &
    0.3077773696572407D+00,-.9878642619619029D+00, &
    0.6952834621750591D+00,0.8445020176382917D+00, &
    0.5386138090792357D+00,-.9518141860165342D+00, &
    -.1344815378332747D+00,-.3695179976651696D+00, &
    0.9873492554611523D+00,0.7062788639769675D+00, &
    -.6456167225977583D+00,-.3396836721033217D+00, &
    -.9362361756962438D+00,-.9626677628820013D+00, &
    0.3015160924588678D+00,0.7067532860197516D+00, &
    -.9571646423196823D+00,0.9839100297235785D+00, &
    -.2419685587001514D+00,0.9438884112343363D+00, &
    -.1742260222577426D+00 /
  data ys / &
    -.4215090372415874D-01,-.3692934551145019D+00, &
    -.1931519618055097D+00,0.3988972577148074D+00, &
    -.1715260024620762D+00,0.9094199619403520D+00, &
    0.6714959797593258D+00,0.3368323178193660D+00, &
    0.6925643585799712D+00,-.3334911148152457D+00, &
    0.1579601812062493D+00,0.7212403727697902D+00, &
    0.7924318088830316D+00,0.4711520853394768D+00, &
    0.2329014884732836D+00,0.7688776471476298D-01, &
    -.2427208512979698D+00,-.9819844652166957D+00, &
    0.6275746962958220D+00,0.3958917640748951D+00, &
    0.8330411855203188D+00,-.4968412520921159D+00, &
    0.4887729302167374D-01,-.3604995467485130D+00, &
    0.5502876418517838D+00,-.1552754382216172D+00, &
    0.5038985691395298D+00,-.3255794611786079D+00, &
    0.3310081213549004D+00,-.6602065173414950D+00, &
    -.4894888916356721D+00,0.5399510111685960D+00, &
    0.7655039704316382D+00,-.5820392397031763D+00, &
    0.1995474060499386D+00,0.8781656258066206D+00, &
    -.1525213978780895D+00,-.5001983735790505D+00, &
    0.7986695769024170D+00,0.4824228603329770D+00, &
    0.1592378639694050D+00,0.1732234025449527D-02, &
    -.9985738973637792D-01,0.5955792594614818D+00, &
    0.3498450621988221D+00,0.2407129566690367D+00, &
    0.9947445171943684D+00,0.6288090977170689D+00, &
    0.7414927225667501D+00,-.9929600347714176D+00, &
    0.3684373427277031D+00,-.9535682594324043D+00, &
    0.3034752604286319D+00,0.8507770288950726D+00, &
    0.6305231708999399D-01,-.8678090617845265D+00, &
    -.4141884020649694D-01,0.9931812444078701D+00, &
    -.6492023240589931D+00,-.6795234046038703D-02, &
    -.7116015253442534D+00,-.5912100888422367D+00, &
    0.9669064714907090D+00,-.9896978591786247D+00, &
    -.6357772860044338D+00,-.2816935374305686D+00, &
    0.9465152907473502D+00,0.9835467134448914D+00, &
    0.5163644022959618D+00,0.9899894348223212D+00, &
    -.9119806917142169D+00,0.9594778630442550D+00, &
    -.8263115619511332D+00,-.4386676054811558D+00, &
    0.9540273279949707D+00,-.7748522899685367D+00, &
    -.7271449178503047D+00,-.8683831780306135D+00, &
    -.9979822139283684D+00,0.1525566715594689D+00, &
    -.3052870542039953D+00,-.8823794421118246D+00, &
    -.6417482148168809D+00,-.3240255976105304D+00, &
    0.6580511661237559D+00,0.6610461063468759D+00, &
    -.9615342517942168D+00,-.8967208208364033D-01, &
    -.4912350350325419D+00,-.8431987564696162D+00, &
    -.7917868223215425D+00,-.9934797396330936D+00, &
    0.4007666180051725D+00,0.8621553422717932D+00, &
    0.9342344412005841D+00,-.9702959974411298D+00, &
    -.9316660034779380D+00,0.7509009750438762D+00, &
    -.5058825420350775D+00,0.7766280231903698D+00, &
    -.1591105418860929D+00,-.7614693278871865D+00, &
    -.6171441191167790D-01,-.9548062395608765D+00, &
    -.8211827496355327D+00,-.9063342142005131D+00, &
    0.9909185703328497D+00,0.9397084665195838D+00, &
    0.8902356582373753D+00,0.9582397465258308D+00, &
    -.9888160497196922D+00,-.6701339599706453D+00, &
    -.9449104256261275D+00,0.9907698250163826D+00, &
    -.8840101170670746D+00,0.9954576420924693D+00, &
    -.9868058887952446D+00,-.9884037853120492D+00, &
    0.8605082508169065D+00,0.1586026433797343D+00, &
    -.7662425769553762D+00,-.9472631440912187D+00, &
    0.5782109944873408D+00,0.7561475499065624D+00, &
    0.9307512366430157D+00,0.8850736835159266D+00, &
    -.4904564371890563D+00 /
  data ws / &
    0.2913207807844331D-01,0.2275827975953009D-01, &
    0.2791249243709578D-01,0.1938452906022129D-01, &
    0.4364917601570797D-01,0.1237342034594484D-01, &
    0.1138865003097454D-01,0.4923704893009620D-01, &
    0.3063932561386288D-01,0.4428512850707964D-01, &
    0.4629109684697653D-01,0.1691765251942423D-01, &
    0.2741532810734821D-01,0.4052530459111174D-01, &
    0.5261109228291892D-01,0.4417466900490126D-01, &
    0.3058410753870397D-01,0.6406355493167926D-02, &
    0.6769861004210370D-02,0.4884106698203454D-01, &
    0.1697345666295625D-01,0.3712422414211005D-01, &
    0.4783538888580537D-01,0.2351215352613192D-01, &
    0.3967485550125242D-01,0.5227057590404430D-01, &
    0.7706038595693665D-02,0.1393888240267354D-01, &
    0.2605232454276587D-01,0.3221602953245650D-02, &
    0.3639783162612337D-01,0.3177013439355753D-01, &
    0.2662783889362856D-01,0.2133361104980210D-01, &
    0.4324098371978138D-01,0.2186171413802145D-01, &
    0.3997793758416952D-01,0.7491762737168889D-02, &
    0.1718989677527449D-01,0.3150610588787735D-01, &
    0.3338285362580999D-01,0.3522688180840185D-01, &
    0.2250843481977659D-01,0.2767390941687784D-01, &
    0.2163076701762542D-01,0.6370202612529778D-02, &
    0.1292816837261490D-02,0.1772036481071653D-01, &
    0.2045516332866545D-01,0.5029942715423091D-02, &
    0.2555860092259501D-01,0.8908434653957875D-02, &
    0.4462964590144296D-01,0.2350361388650933D-01, &
    0.1300162042596963D-01,0.1619762342723978D-01, &
    0.3633406947058673D-01,0.4148575443002976D-02, &
    0.3043458935989431D-01,0.3780056077982088D-01, &
    0.2670761755118099D-01,0.4679248624556967D-01, &
    0.6655017468306593D-02,0.1016870301128343D-02, &
    0.3956208492384800D-01,0.4422000056024995D-01, &
    0.1600493333371462D-02,0.9542615574085509D-02, &
    0.5101463103747718D-01,0.2316486911009150D-02, &
    0.2082363944511977D-01,0.1450858408533323D-01, &
    0.2593506316945297D-01,0.4854771494052668D-01, &
    0.1406093874479688D-01,0.2980564799727076D-01, &
    0.4032704717924811D-01,0.2836242586524349D-01, &
    0.1662837317311870D-02,0.3331747639887452D-01, &
    0.1737244435785181D-01,0.1818621007558914D-01, &
    0.4099371056923931D-01,0.5481494042070843D-01, &
    0.4070179363666812D-01,0.4218182579123851D-01, &
    0.8533899793279886D-02,0.5949667491635394D-02, &
    0.2693305692079543D-01,0.3035811941473991D-01, &
    0.1815553496055310D-01,0.3365556943518273D-02, &
    0.5641437853076681D-02,0.1997142107835644D-01, &
    0.1803786451914521D-01,0.2269810849285899D-02, &
    0.1824056247133745D-01,0.4961745614234228D-02, &
    0.5229975396089418D-02,0.3090226019029179D-01, &
    0.4530557980884387D-02,0.1260165841456744D-01, &
    0.2688472868433625D-01,0.6305207130138189D-02, &
    0.4114195161424335D-02,0.8573396052945570D-02, &
    0.6467557193927746D-02,0.2703578258059107D-02, &
    0.1975997243239372D-01,0.9381005409995410D-02, &
    0.6472606174579578D-02,0.1382406712148664D-01, &
    0.1805324127586059D-01,0.6329200107849942D-02, &
    0.3895024567992489D-02,0.3809507629042853D-02, &
    0.6517542895566797D-02,0.7169776873936058D-02, &
    0.1060178841340027D-01,0.1641915380777178D-01, &
    0.3678722762764779D-01,0.1379818565014169D-01, &
    0.1123917596421086D-01,0.6410312514956618D-02, &
    0.1884563206279892D-01,0.8912626526178684D-02, &
    0.4962602708111043D-01 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
  w(1:n) = ws(1:n)

  return
end
subroutine rule27 ( n, x, w )

!*****************************************************************************80
!
!! RULE27 returns the rule of degree 27.
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
  parameter ( ns = 132 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
    -.3179234344617641D+00,0.9945426115270097D+00, &
    0.3179234344617642D+00,-.9945426115270097D+00, &
    -.9658581524130557D+00,0.9842821312283567D+00, &
    0.9658581524130559D+00,-.9842821312283565D+00, &
    0.4546608217543911D+00,0.9794570195190775D+00, &
    -.4546608217543910D+00,-.9794570195190775D+00, &
    0.9203445363225692D+00,0.9966680243748195D+00, &
    -.9203445363225690D+00,-.9966680243748197D+00, &
    0.5921826557247571D+00,0.9227007102569557D+00, &
    -.5921826557247569D+00,-.9227007102569557D+00, &
    -.6265556238656111D+00,0.9943315661391944D+00, &
    0.6265556238656114D+00,-.9943315661391944D+00, &
    -.9106466233445932D+00,0.9127106311205702D+00, &
    0.9106466233445935D+00,-.9127106311205699D+00, &
    -.4796281374834562D+00,0.9651530210115425D+00, &
    0.4796281374834563D+00,-.9651530210115425D+00, &
    0.6298732844502039D+00,0.9921721529860120D+00, &
    -.6298732844502036D+00,-.9921721529860120D+00, &
    -.8335744040052592D+00,0.9824660135148449D+00, &
    0.8335744040052594D+00,-.9824660135148449D+00, &
    0.1959395767367455D+00,0.9899174626675955D+00, &
    -.1959395767367454D+00,-.9899174626675955D+00, &
    -.1386302484544642D+00,0.9830581303940688D+00, &
    0.1386302484544643D+00,-.9830581303940688D+00, &
    -.7022171903082602D+00,0.9292585099108952D+00, &
    0.7022171903082605D+00,-.9292585099108952D+00, &
    0.7970439287784883D+00,0.9675649927064782D+00, &
    -.7970439287784881D+00,-.9675649927064782D+00, &
    -.8020178388613470D+00,0.8160591260895965D+00, &
    0.8020178388613470D+00,-.8160591260895965D+00, &
    -.5398233377470387D+00,0.8364692379579785D+00, &
    0.5398233377470387D+00,-.8364692379579785D+00, &
    -.1520649596611693D+00,0.8369974777769484D+00, &
    0.1520649596611695D+00,-.8369974777769484D+00, &
    -.2998875490507344D+00,0.3409328206751328D+00, &
    0.2998875490507344D+00,-.3409328206751328D+00, &
    -.3209420000531037D+00,0.9175861963438381D+00, &
    0.3209420000531039D+00,-.9175861963438381D+00, &
    -.9020677393268002D-01,0.1670667038911821D+00, &
    0.9020677393268005D-01,-.1670667038911821D+00, &
    0.1926515070565072D+00,0.5293872532788142D+00, &
    -.1926515070565071D+00,-.5293872532788142D+00, &
    0.4976080851052046D+00,0.7953917795116896D+00, &
    -.4976080851052045D+00,-.7953917795116896D+00, &
    0.3002782637358375D-01,0.9362295643614138D+00, &
    -.3002782637358363D-01,-.9362295643614138D+00, &
    0.6881617137006012D+00,0.8842713205559528D+00, &
    -.6881617137006012D+00,-.8842713205559528D+00, &
    0.2200611683790181D+00,0.8481900686404982D+00, &
    -.2200611683790180D+00,-.8481900686404982D+00, &
    0.4531153802420229D-01,0.3769373245611116D+00, &
    -.4531153802420225D-01,-.3769373245611116D+00, &
    -.4941549461445193D+00,0.5167583593472795D+00, &
    0.4941549461445194D+00,-.5167583593472795D+00, &
    0.3405123948054840D+00,0.6649111597000639D+00, &
    -.3405123948054839D+00,-.6649111597000639D+00, &
    -.1521097283009047D+00,0.5602567958473701D+00, &
    0.1521097283009048D+00,-.5602567958473701D+00, &
    0.3710843473830662D+00,0.9336952377043988D+00, &
    -.3710843473830661D+00,-.9336952377043988D+00, &
    -.3536770731498163D+00,0.7123617422035845D+00, &
    0.3536770731498164D+00,-.7123617422035845D+00, &
    -.6668117984035637D+00,0.6750511711635266D+00, &
    0.6668117984035637D+00,-.6750511711635266D+00, &
    0.3880331998977081D-01,0.7201416766661869D+00, &
    -.3880331998977073D-01,-.7201416766661869D+00 /
  data ys / &
    -.9945426115270097D+00,-.3179234344617641D+00, &
    0.9945426115270097D+00,0.3179234344617642D+00, &
    -.9842821312283568D+00,-.9658581524130558D+00, &
    0.9842821312283566D+00,0.9658581524130561D+00, &
    -.9794570195190775D+00,0.4546608217543910D+00, &
    0.9794570195190775D+00,-.4546608217543909D+00, &
    -.9966680243748194D+00,0.9203445363225691D+00, &
    0.9966680243748196D+00,-.9203445363225689D+00, &
    -.9227007102569557D+00,0.5921826557247570D+00, &
    0.9227007102569557D+00,-.5921826557247568D+00, &
    -.9943315661391944D+00,-.6265556238656113D+00, &
    0.9943315661391944D+00,0.6265556238656115D+00, &
    -.9127106311205703D+00,-.9106466233445933D+00, &
    0.9127106311205700D+00,0.9106466233445936D+00, &
    -.9651530210115425D+00,-.4796281374834563D+00, &
    0.9651530210115425D+00,0.4796281374834564D+00, &
    -.9921721529860120D+00,0.6298732844502037D+00, &
    0.9921721529860120D+00,-.6298732844502035D+00, &
    -.9824660135148449D+00,-.8335744040052593D+00, &
    0.9824660135148449D+00,0.8335744040052595D+00, &
    -.9899174626675955D+00,0.1959395767367454D+00, &
    0.9899174626675955D+00,-.1959395767367453D+00, &
    -.9830581303940688D+00,-.1386302484544642D+00, &
    0.9830581303940688D+00,0.1386302484544643D+00, &
    -.9292585099108952D+00,-.7022171903082604D+00, &
    0.9292585099108952D+00,0.7022171903082606D+00, &
    -.9675649927064782D+00,0.7970439287784882D+00, &
    0.9675649927064782D+00,-.7970439287784880D+00, &
    -.8160591260895965D+00,-.8020178388613470D+00, &
    0.8160591260895965D+00,0.8020178388613470D+00, &
    -.8364692379579785D+00,-.5398233377470387D+00, &
    0.8364692379579785D+00,0.5398233377470387D+00, &
    -.8369974777769484D+00,-.1520649596611694D+00, &
    0.8369974777769484D+00,0.1520649596611695D+00, &
    -.3409328206751328D+00,-.2998875490507344D+00, &
    0.3409328206751328D+00,0.2998875490507344D+00, &
    -.9175861963438381D+00,-.3209420000531038D+00, &
    0.9175861963438381D+00,0.3209420000531039D+00, &
    -.1670667038911821D+00,-.9020677393268003D-01, &
    0.1670667038911821D+00,0.9020677393268006D-01, &
    -.5293872532788142D+00,0.1926515070565072D+00, &
    0.5293872532788142D+00,-.1926515070565071D+00, &
    -.7953917795116896D+00,0.4976080851052045D+00, &
    0.7953917795116896D+00,-.4976080851052044D+00, &
    -.9362295643614138D+00,0.3002782637358369D-01, &
    0.9362295643614138D+00,-.3002782637358357D-01, &
    -.8842713205559528D+00,0.6881617137006012D+00, &
    0.8842713205559528D+00,-.6881617137006012D+00, &
    -.8481900686404982D+00,0.2200611683790180D+00, &
    0.8481900686404982D+00,-.2200611683790179D+00, &
    -.3769373245611116D+00,0.4531153802420227D-01, &
    0.3769373245611116D+00,-.4531153802420223D-01, &
    -.5167583593472795D+00,-.4941549461445193D+00, &
    0.5167583593472795D+00,0.4941549461445194D+00, &
    -.6649111597000639D+00,0.3405123948054840D+00, &
    0.6649111597000639D+00,-.3405123948054838D+00, &
    -.5602567958473701D+00,-.1521097283009047D+00, &
    0.5602567958473701D+00,0.1521097283009048D+00, &
    -.9336952377043988D+00,0.3710843473830662D+00, &
    0.9336952377043988D+00,-.3710843473830661D+00, &
    -.7123617422035845D+00,-.3536770731498164D+00, &
    0.7123617422035845D+00,0.3536770731498165D+00, &
    -.6750511711635266D+00,-.6668117984035637D+00, &
    0.6750511711635266D+00,0.6668117984035637D+00, &
    -.7201416766661869D+00,0.3880331998977077D-01, &
    0.7201416766661869D+00,-.3880331998977068D-01 /
  data ws / &
    0.2672899331427035D-02,0.2672899331427035D-02, &
    0.2672899331427035D-02,0.2672899331427035D-02, &
    0.2445906596530064D-02,0.2445906596530064D-02, &
    0.2445906596530064D-02,0.2445906596530064D-02, &
    0.5784555125932030D-02,0.5784555125932030D-02, &
    0.5784555125932030D-02,0.5784555125932030D-02, &
    0.1774682379035372D-02,0.1774682379035372D-02, &
    0.1774682379035372D-02,0.1774682379035372D-02, &
    0.9666652283416993D-02,0.9666652283416993D-02, &
    0.9666652283416993D-02,0.9666652283416993D-02, &
    0.3475754833559290D-02,0.3475754833559290D-02, &
    0.3475754833559290D-02,0.3475754833559290D-02, &
    0.9738340699044858D-02,0.9738340699044858D-02, &
    0.9738340699044858D-02,0.9738340699044858D-02, &
    0.9913445575624551D-02,0.9913445575624551D-02, &
    0.9913445575624551D-02,0.9913445575624551D-02, &
    0.4024564097795350D-02,0.4024564097795350D-02, &
    0.4024564097795350D-02,0.4024564097795350D-02, &
    0.5467642658052182D-02,0.5467642658052182D-02, &
    0.5467642658052182D-02,0.5467642658052182D-02, &
    0.6791307131143990D-02,0.6791307131143990D-02, &
    0.6791307131143990D-02,0.6791307131143990D-02, &
    0.7849471971542231D-02,0.7849471971542231D-02, &
    0.7849471971542231D-02,0.7849471971542231D-02, &
    0.1481562369739479D-01,0.1481562369739479D-01, &
    0.1481562369739479D-01,0.1481562369739479D-01, &
    0.8652776980870136D-02,0.8652776980870136D-02, &
    0.8652776980870136D-02,0.8652776980870136D-02, &
    0.1938931217339045D-01,0.1938931217339045D-01, &
    0.1938931217339045D-01,0.1938931217339045D-01, &
    0.2728047644809397D-01,0.2728047644809397D-01, &
    0.2728047644809397D-01,0.2728047644809397D-01, &
    0.2833042504936177D-01,0.2833042504936177D-01, &
    0.2833042504936177D-01,0.2833042504936177D-01, &
    0.5141433190365619D-01,0.5141433190365619D-01, &
    0.5141433190365619D-01,0.5141433190365619D-01, &
    0.1775207295130232D-01,0.1775207295130232D-01, &
    0.1775207295130232D-01,0.1775207295130232D-01, &
    0.4944421261124536D-01,0.4944421261124536D-01, &
    0.4944421261124536D-01,0.4944421261124536D-01, &
    0.3632695571196309D-01,0.3632695571196309D-01, &
    0.3632695571196309D-01,0.3632695571196309D-01, &
    0.2905709329104403D-01,0.2905709329104403D-01, &
    0.2905709329104403D-01,0.2905709329104403D-01, &
    0.1861253661440120D-01,0.1861253661440120D-01, &
    0.1861253661440120D-01,0.1861253661440120D-01, &
    0.1362514538944166D-01,0.1362514538944166D-01, &
    0.1362514538944166D-01,0.1362514538944166D-01, &
    0.2778931208298291D-01,0.2778931208298291D-01, &
    0.2778931208298291D-01,0.2778931208298291D-01, &
    0.4496161760244370D-01,0.4496161760244370D-01, &
    0.4496161760244370D-01,0.4496161760244370D-01, &
    0.4391754850935400D-01,0.4391754850935400D-01, &
    0.4391754850935400D-01,0.4391754850935400D-01, &
    0.3461629438577501D-01,0.3461629438577501D-01, &
    0.3461629438577501D-01,0.3461629438577501D-01, &
    0.4733898929183223D-01,0.4733898929183223D-01, &
    0.4733898929183223D-01,0.4733898929183223D-01, &
    0.1448889464186536D-01,0.1448889464186536D-01, &
    0.1448889464186536D-01,0.1448889464186536D-01, &
    0.3863763742526743D-01,0.3863763742526743D-01, &
    0.3863763742526743D-01,0.3863763742526743D-01, &
    0.3295515782263461D-01,0.3295515782263461D-01, &
    0.3295515782263461D-01,0.3295515782263461D-01, &
    0.3809514391912308D-01,0.3809514391912308D-01, &
    0.3809514391912308D-01,0.3809514391912308D-01 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
  w(1:n) = ws(1:n)

  return
end
subroutine rule28 ( n, x, w )

!*****************************************************************************80
!
!! RULE28 returns the rule of degree 28.
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
  parameter ( ns = 147 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
    0.2827621856762107D+00,-.8043038962721508D+00, &
    -.3395780944817125D+00,0.9664085101515743D+00, &
    -.1096329656209190D-01,0.8193786706976756D+00, &
    -.9286317198271657D+00,0.4198022792026188D+00, &
    -.8604918125918429D+00,0.8109783092388664D+00, &
    0.5614606479838853D+00,0.1250139096754087D-01, &
    0.6890874578865651D+00,-.8003785261499256D+00, &
    -.7186772992712774D+00,0.2097379198022608D-01, &
    -.9904175004752394D+00,0.5247683324321177D+00, &
    0.9879057318966782D+00,0.9872995514825285D+00, &
    -.2010899760026548D+00,-.7150754139309137D+00, &
    -.9744175518522036D+00,-.2743957404149748D+00, &
    -.8801155850915744D-01,-.9120488211036881D+00, &
    0.5997911603089924D-01,-.9822903323396509D+00, &
    0.9933058426309856D+00,-.1094425840746250D+00, &
    0.8016032845806299D+00,-.3785896040093871D+00, &
    0.9908494000362051D+00,-.9920099650417782D+00, &
    -.5334142633484058D+00,-.6081971936243052D+00, &
    -.9913559247973139D+00,-.4420258077139650D+00, &
    0.9904922021473850D+00,-.2701059581013934D+00, &
    -.6762870118531359D+00,0.4550203878192706D+00, &
    -.9933274734742550D+00,0.1025031015215653D+00, &
    -.9908021888968861D+00,0.4018136560549200D-02, &
    0.1830465948969627D+00,-.9190332253043175D+00, &
    -.7967293798867977D+00,0.8910586530553062D+00, &
    0.9738677077447094D+00,0.4922211332598602D+00, &
    -.9935974988322385D+00,-.7469015595759197D+00, &
    -.7474851457426346D+00,-.8461768034166064D+00, &
    0.7263311602661456D+00,0.1838655693690960D+00, &
    -.8924925604346934D+00,0.3473229569871696D+00, &
    0.7888084262939273D+00,-.1622547625661196D+00, &
    0.1708950639909230D+00,-.9858834062192522D+00, &
    0.8595474037303855D+00,-.2487721441258377D+00, &
    0.9897369384324803D+00,-.8592565754231390D+00, &
    0.3552890037180957D+00,0.9343486754942414D+00, &
    -.5982760380368914D+00,0.9012497934650560D+00, &
    -.8867898943082418D+00,-.2973823530897202D+00, &
    0.2929780512625694D+00,0.5109886162010564D+00, &
    -.6362138293509048D+00,0.9840555620580882D+00, &
    0.6242255274464619D+00,0.9500492760473710D+00, &
    0.6206677950945041D+00,0.3667200892151265D+00, &
    -.3354257519635134D+00,0.6532050712129780D+00, &
    0.8340712667685216D+00,0.9517120425530485D+00, &
    0.4939764190001616D+00,0.8519117390868273D+00, &
    -.4641713197529147D+00,0.3096864797777032D+00, &
    -.9951406550164114D+00,0.6849334791605266D+00, &
    0.5377487266764005D+00,0.6596506837798173D+00, &
    -.6190982139272722D+00,0.9676681958645598D+00, &
    -.8663734426507712D+00,0.9229408505602535D+00, &
    0.8925298503917507D+00,0.1376340175855351D+00, &
    -.4255923534036878D+00,-.5069417238664446D+00, &
    -.9581264401591332D+00,0.7956423904664977D+00, &
    -.9438198089736733D+00,-.9602193183322451D+00, &
    -.9546324794056378D+00,0.7865853669573317D+00, &
    0.7640178616435913D+00,-.9037187541181603D-01, &
    0.6583886775017005D+00,0.9394184495408773D+00, &
    0.1210974568433837D+00,0.3181846883529323D+00, &
    -.9476649688000099D+00,-.7447346202738520D+00, &
    0.7669083321105266D+00,0.3177180418892223D+00, &
    0.9005374034994568D+00,0.4905235558364557D+00, &
    -.2799102574207112D+00,-.7756668408683119D+00, &
    -.7847585523489610D+00,-.5947448678197160D+00, &
    -.6835954896209653D-01,-.4603444798372620D+00, &
    0.1267789889405305D+00,0.9957007235251634D+00, &
    -.9555617449567378D+00,0.4926175951876377D+00, &
    0.9580744421864001D+00,0.6544409855907934D+00, &
    -.2589485669037206D+00,-.4625383187120728D+00, &
    0.8779137327105354D+00,-.8753270142023138D+00, &
    -.7680237343653598D+00,-.4646141113930034D+00, &
    -.6237643000374615D+00,-.1004225708457810D+00, &
    0.2934408497776114D+00,-.8838031019815079D+00, &
    -.8252198025376763D-01,0.1013709068378068D+00, &
    -.6355163316600663D+00,-.2814473462899598D+00, &
    0.9991999747547105D+00 /
  data ys / &
    -.2146915742023843D+00,-.3602259227928870D+00, &
    -.2366070789246893D+00,-.1067737835329393D+00, &
    -.7832467667692294D-01,0.5090533388398083D-02, &
    0.5655320173536176D-02,-.9091460408400718D+00, &
    -.3610436689009546D+00,0.1173540113566495D+00, &
    -.7580822207698504D+00,0.7636857906206649D+00, &
    -.6742700466367159D+00,0.3605821467647248D+00, &
    0.2565264593758684D+00,0.8682397249203467D+00, &
    -.9887795883901583D+00,-.8777099834380453D+00, &
    -.9937283708213308D+00,0.9906296802548810D+00, &
    0.8788092453036578D+00,0.9706849956765652D+00, &
    -.7829696251737753D-01,0.9937649402523892D+00, &
    -.1524877190213388D+00,-.2242135513466236D+00, &
    0.9919534212785273D+00,0.9960588898544035D+00, &
    -.5034894383669408D+00,0.9599951355029216D+00, &
    -.5645937597081716D+00,0.9447996831444674D+00, &
    0.2711989824047218D+00,0.1447359435070455D+00, &
    0.9871535260388329D+00,0.9140753757822478D+00, &
    -.2960350108035981D+00,0.8371402678277077D+00, &
    -.7650309750102933D+00,-.3188681363434392D+00, &
    -.9938368658433656D+00,-.6339545710762906D+00, &
    -.6212095493273496D+00,-.9934382054756534D+00, &
    0.4715389357436094D+00,-.7215460671816014D+00, &
    0.6773640866690520D+00,0.9779955899159012D+00, &
    0.9970513707078691D+00,-.1551145670991118D+00, &
    -.3024178806712221D+00,-.9926567100374114D+00, &
    -.8660993570031557D+00,-.4956056862948234D+00, &
    0.8382998942855415D+00,0.9259687805728221D+00, &
    -.8351882927550272D+00,0.9386358290968080D+00, &
    -.9900384261478909D+00,-.4464745325070345D+00, &
    -.9936162118016632D+00,-.8235872679034352D+00, &
    -.5983594352543780D+00,0.9398154839397104D+00, &
    -.7371773517980565D+00,0.7366500643247871D+00, &
    0.8982625204679442D+00,0.1211489804261566D+00, &
    0.9859353129167986D+00,-.8465334600907201D+00, &
    0.7342041846853576D+00,-.4349009915613299D+00, &
    0.5008348384608579D+00,-.9942925485328575D+00, &
    -.9626593056228334D+00,-.2709700923096490D+00, &
    0.5944472663027801D-01,-.9248481403512147D+00, &
    -.4757878778805171D+00,0.9921234070894396D-01, &
    0.9864359054124004D+00,0.5564466223050852D+00, &
    -.9075561035062022D+00,0.6130603473347437D+00, &
    -.9196402734279348D+00,-.6285797630023995D+00, &
    0.7360652264674404D+00,0.9946281004658378D+00, &
    -.1056502846403645D+00,0.8343858336015567D+00, &
    0.7795434558480316D+00,0.2437361149690468D+00, &
    0.4083791315159580D+00,-.8237952273107227D-01, &
    -.6450626080823243D+00,0.4764961946576270D+00, &
    0.7429354103100736D+00,-.9752458082257867D+00, &
    0.3137605530793695D+00,0.7602258278505522D-01, &
    0.6028197206610887D+00,-.9665994542595457D+00, &
    -.9448687562971092D+00,0.7450892979512898D+00, &
    0.8580812769827701D+00,0.6469685322672528D+00, &
    -.4730667721016463D+00,0.4691067254414554D+00, &
    0.9468223400568799D+00,0.5747271977509141D+00, &
    0.8487224420002684D+00,0.9604420521431418D+00, &
    0.4325756220233750D+00,-.1154193690840114D+00, &
    0.2996859346038543D+00,0.6079262704780987D+00, &
    -.3014753479396839D+00,0.2694764821728903D+00, &
    0.6210587156046703D+00,0.9250623306784818D+00, &
    0.4091950530277537D+00,-.1149400908487048D+00, &
    -.9509596164554358D+00,0.4489210023756193D+00, &
    0.2491367754635779D+00,0.2328542094943754D+00, &
    -.3147181280829211D+00,0.6575404381717325D+00, &
    -.7572545223612748D+00,0.8744761196977546D-01, &
    0.7809511457782177D+00,-.9573612252124380D+00, &
    0.5984333435621010D-01,-.4795365437914892D+00, &
    0.8758201084343584D+00,-.6288667697841542D+00, &
    -.7704960778314179D+00,-.7807547746986493D+00, &
    -.2948221971565224D+00,-.9595773298541288D+00, &
    -.7859464731741874D+00,-.8701241408788146D+00, &
    -.4848613986697829D+00,-.8903571922698699D+00, &
    -.8832911479917025D+00,-.6435327316304572D+00, &
    -.5075800418230667D-01 /
  data ws / &
    0.1740144063407695D-02,0.6784563755290567D-02, &
    0.1209466540188899D-01,0.5276845120506419D-02, &
    0.2120637597472411D-01,0.1500950169216209D-01, &
    0.1016605179698056D-01,0.1052237804962158D-01, &
    0.1389531092331450D-01,0.1799231574573814D-01, &
    0.1681749672744976D-01,0.2190516172058484D-01, &
    0.1802050074136916D-01,0.1929827700816552D-01, &
    0.2322655354121691D-01,0.1715984331158577D-01, &
    0.7585152723116851D-03,0.1423586238543194D-01, &
    0.6209204642340611D-03,0.8055582944619972D-03, &
    0.1686431398199053D-01,0.6130199135277467D-02, &
    0.8266675752855135D-02,0.3947441053087870D-02, &
    0.3763432904885534D-01,0.1544537020393182D-01, &
    0.4895887118324937D-02,0.5766157576611153D-03, &
    0.3911408899973994D-02,0.1134055149899166D-01, &
    0.2045540821981484D-01,0.1254460489172730D-01, &
    0.5370936278809426D-02,0.5162980339438905D-02, &
    0.5510957955189620D-02,0.1348681146014646D-01, &
    0.5293461261550956D-02,0.2135158963906704D-01, &
    0.3872040956962797D-02,0.4041863730898066D-01, &
    0.3480075985572175D-02,0.3082881220603644D-01, &
    0.3934841673430547D-02,0.4955048643733777D-02, &
    0.5332713420324274D-02,0.3127092112918890D-01, &
    0.3263868066569824D-01,0.3643906126193626D-02, &
    0.1773410002914162D-02,0.2031653637703585D-01, &
    0.9778965531187402D-02,0.4720821658621592D-02, &
    0.2492177802034716D-02,0.2644508244904856D-01, &
    0.1656291578773864D-01,0.9176500057842589D-02, &
    0.1732347887472145D-01,0.1561854636309510D-01, &
    0.2892151505244423D-02,0.3887473612274506D-01, &
    0.3107151229520027D-02,0.2613845455837795D-01, &
    0.3698667210865119D-01,0.2566493481700192D-02, &
    0.1615348439627276D-01,0.3075142974941159D-01, &
    0.2904460597417912D-02,0.2398826862979595D-01, &
    0.7145392996633393D-02,0.8978366822810722D-02, &
    0.2612889620260116D-01,0.1874378199539706D-01, &
    0.1917559308928981D-01,0.4661196658021693D-02, &
    0.1235879177909568D-01,0.4010025698621002D-01, &
    0.3740253247098928D-01,0.3148001303878885D-02, &
    0.3330951974159902D-01,0.1500177718625442D-01, &
    0.6070126982071918D-02,0.3772728641321498D-01, &
    0.1934239152098538D-01,0.2980908782028305D-01, &
    0.1060149985804505D-01,0.1169140727671868D-01, &
    0.2913863637730209D-01,0.2512926081791887D-02, &
    0.4408214653268561D-01,0.2616523021560373D-01, &
    0.2824238355893381D-02,0.3534124084616964D-01, &
    0.3855284013804392D-01,0.3769601120920466D-01, &
    0.3068556194531641D-01,0.1107117178829623D-01, &
    0.1675502525295356D-01,0.4250234458185585D-02, &
    0.2160027234544727D-01,0.4964511475611218D-01, &
    0.3661675186805007D-01,0.1113946092997709D-01, &
    0.4594039996241282D-02,0.2058836038539646D-01, &
    0.8608231530163511D-02,0.1071439010120374D-01, &
    0.1325231032613585D-01,0.2793585524817994D-01, &
    0.1060087465526644D-01,0.4186289109506627D-01, &
    0.2038937560848930D-01,0.4779376144301057D-02, &
    0.4600352201496827D-01,0.4850236286164095D-01, &
    0.1563580434067219D-01,0.2738173661329469D-01, &
    0.3152680760913545D-01,0.4699759394230683D-01, &
    0.1758154363368765D-01,0.1705163560097511D-01, &
    0.4564749915571795D-01,0.3274246294590765D-01, &
    0.9932190596257133D-02,0.3764556185875566D-01, &
    0.5058095830353233D-01,0.4537296603002153D-01, &
    0.4951708484325965D-01,0.3279405659962938D-02, &
    0.9987676851635598D-02,0.4573491590126628D-01, &
    0.9304988468918474D-02,0.1136613167624352D-01, &
    0.5119306902579393D-01,0.4198377464395852D-01, &
    0.1182329164018337D-01,0.1992962132997380D-01, &
    0.2179036392120167D-01,0.2957452226482192D-01, &
    0.3999530391333647D-01,0.1478968751484580D-01, &
    0.3170728715126719D-01,0.1199170942901081D-01, &
    0.4711944799135396D-01,0.2445849079621999D-01, &
    0.1955624669135905D-01,0.4043330460830744D-01, &
    0.2815879734180029D-02 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
  w(1:n) = ws(1:n)

  return
end
subroutine rule29 ( n, x, w )

!*****************************************************************************80
!
!! RULE29 returns the rule of degree 29.
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
  parameter ( ns = 152 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
    -.9957609056803307D+00,0.9554778819247284D+00, &
    0.9957609056803309D+00,-.9554778819247282D+00, &
    -.8018419403764031D+00,0.9936244517402659D+00, &
    0.8018419403764033D+00,-.9936244517402659D+00, &
    -.9579902382554616D+00,0.9905651653257478D+00, &
    0.9579902382554618D+00,-.9905651653257476D+00, &
    0.2638223412555635D-01,0.9935202992997069D+00, &
    -.2638223412555623D-01,-.9935202992997069D+00, &
    -.5066682706262957D+00,0.9913300131756428D+00, &
    0.5066682706262959D+00,-.9913300131756428D+00, &
    -.2298993811511364D+00,0.9820954997507334D+00, &
    0.2298993811511365D+00,-.9820954997507334D+00, &
    0.4377228178889339D+00,0.9940035328985276D+00, &
    -.4377228178889338D+00,-.9940035328985276D+00, &
    -.6971536278750922D+00,0.9699469973610456D+00, &
    0.6971536278750924D+00,-.9699469973610456D+00, &
    -.1287796452538595D+00,0.9374257301963881D+00, &
    0.1287796452538596D+00,-.9374257301963881D+00, &
    0.2219765871034010D+00,0.9669818379796637D+00, &
    -.2219765871034008D+00,-.9669818379796637D+00, &
    0.8793284704221047D+00,0.9627913549967865D+00, &
    -.8793284704221045D+00,-.9627913549967865D+00, &
    -.4224857795905392D+00,0.8997395988035505D+00, &
    0.4224857795905393D+00,-.8997395988035505D+00, &
    0.7561015490560310D+00,0.9929163478894405D+00, &
    -.7561015490560308D+00,-.9929163478894405D+00, &
    0.2640620995843525D+00,0.8673709698403427D+00, &
    -.2640620995843524D+00,-.8673709698403427D+00, &
    -.8876763767236916D+00,0.9499938690143425D+00, &
    0.8876763767236918D+00,-.9499938690143425D+00, &
    0.6107736243515250D+00,0.9600495691709722D+00, &
    -.6107736243515248D+00,-.9600495691709722D+00, &
    -.7868750412424903D+00,0.8719483642340994D+00, &
    0.7868750412424903D+00,-.8719483642340994D+00, &
    -.1141990095980696D-01,0.9167780797645280D+00, &
    0.1141990095980707D-01,-.9167780797645280D+00, &
    0.6200952646798407D+00,0.8398067949907553D+00, &
    -.6200952646798407D+00,-.8398067949907553D+00, &
    -.4280702467551835D+00,0.3950643020883297D+00, &
    0.4280702467551835D+00,-.3950643020883297D+00, &
    0.3401810242198736D+00,0.6973120201514252D+00, &
    -.3401810242198735D+00,-.6973120201514252D+00, &
    -.2617868665129255D+00,0.2355937519408776D+00, &
    0.2617868665129255D+00,-.2355937519408775D+00, &
    0.1411576681950311D+00,0.8161158957508043D+00, &
    -.1411576681950309D+00,-.8161158957508043D+00, &
    -.3695797105378154D-01,0.6981295844594165D+00, &
    0.3695797105378162D-01,-.6981295844594165D+00, &
    -.6312274766498698D+00,0.9353634542809267D+00, &
    0.6312274766498700D+00,-.9353634542809267D+00, &
    -.1150463641736545D+00,0.9674061994537117D-01, &
    0.1150463641736545D+00,-.9674061994537114D-01, &
    -.3747271550737231D+00,0.9438572336357420D+00, &
    0.3747271550737232D+00,-.9438572336357420D+00, &
    0.5008869009053424D+00,0.7904714944143627D+00, &
    -.5008869009053424D+00,-.7904714944143627D+00, &
    0.1608699923234913D+00,0.5513908795637696D+00, &
    -.1608699923234913D+00,-.5513908795637696D+00, &
    -.5657668007893579D+00,0.8210097847827826D+00, &
    0.5657668007893579D+00,-.8210097847827826D+00, &
    0.7712590259177344D+00,0.8970730060299638D+00, &
    -.7712590259177344D+00,-.8970730060299638D+00, &
    0.4163780011554897D+00,0.9113544664316420D+00, &
    -.4163780011554896D+00,-.9113544664316420D+00, &
    -.5836842069804584D+00,0.5579917371493338D+00, &
    0.5836842069804584D+00,-.5579917371493338D+00, &
    -.2215359986069006D+00,0.8254893061838994D+00, &
    0.2215359986069007D+00,-.8254893061838994D+00, &
    -.4009765829120037D+00,0.6957123574117627D+00, &
    0.4009765829120038D+00,-.6957123574117627D+00, &
    -.2343918950839540D-01,0.3763993540852892D+00, &
    0.2343918950839545D-01,-.3763993540852892D+00, &
    -.7105079759089011D+00,0.7179044391714098D+00, &
    0.7105079759089011D+00,-.7179044391714098D+00, &
    -.2200751569931638D+00,0.5431094815533654D+00, &
    0.2200751569931638D+00,-.5431094815533654D+00 /
  data ys / &
    -.9554778819247285D+00,-.9957609056803308D+00, &
    0.9554778819247283D+00,0.9957609056803310D+00, &
    -.9936244517402659D+00,-.8018419403764032D+00, &
    0.9936244517402659D+00,0.8018419403764034D+00, &
    -.9905651653257479D+00,-.9579902382554617D+00, &
    0.9905651653257477D+00,0.9579902382554619D+00, &
    -.9935202992997069D+00,0.2638223412555629D-01, &
    0.9935202992997069D+00,-.2638223412555617D-01, &
    -.9913300131756428D+00,-.5066682706262958D+00, &
    0.9913300131756428D+00,0.5066682706262960D+00, &
    -.9820954997507334D+00,-.2298993811511365D+00, &
    0.9820954997507334D+00,0.2298993811511366D+00, &
    -.9940035328985276D+00,0.4377228178889339D+00, &
    0.9940035328985276D+00,-.4377228178889337D+00, &
    -.9699469973610456D+00,-.6971536278750923D+00, &
    0.9699469973610456D+00,0.6971536278750925D+00, &
    -.9374257301963881D+00,-.1287796452538595D+00, &
    0.9374257301963881D+00,0.1287796452538596D+00, &
    -.9669818379796637D+00,0.2219765871034009D+00, &
    0.9669818379796637D+00,-.2219765871034008D+00, &
    -.9627913549967865D+00,0.8793284704221046D+00, &
    0.9627913549967865D+00,-.8793284704221044D+00, &
    -.8997395988035505D+00,-.4224857795905393D+00, &
    0.8997395988035505D+00,0.4224857795905394D+00, &
    -.9929163478894405D+00,0.7561015490560309D+00, &
    0.9929163478894405D+00,-.7561015490560307D+00, &
    -.8673709698403427D+00,0.2640620995843524D+00, &
    0.8673709698403427D+00,-.2640620995843523D+00, &
    -.9499938690143425D+00,-.8876763767236917D+00, &
    0.9499938690143425D+00,0.8876763767236919D+00, &
    -.9600495691709722D+00,0.6107736243515249D+00, &
    0.9600495691709722D+00,-.6107736243515247D+00, &
    -.8719483642340994D+00,-.7868750412424903D+00, &
    0.8719483642340994D+00,0.7868750412424903D+00, &
    -.9167780797645280D+00,-.1141990095980701D-01, &
    0.9167780797645280D+00,0.1141990095980712D-01, &
    -.8398067949907553D+00,0.6200952646798407D+00, &
    0.8398067949907553D+00,-.6200952646798407D+00, &
    -.3950643020883297D+00,-.4280702467551835D+00, &
    0.3950643020883297D+00,0.4280702467551835D+00, &
    -.6973120201514252D+00,0.3401810242198735D+00, &
    0.6973120201514252D+00,-.3401810242198734D+00, &
    -.2355937519408776D+00,-.2617868665129255D+00, &
    0.2355937519408775D+00,0.2617868665129255D+00, &
    -.8161158957508043D+00,0.1411576681950310D+00, &
    0.8161158957508043D+00,-.1411576681950309D+00, &
    -.6981295844594165D+00,-.3695797105378158D-01, &
    0.6981295844594165D+00,0.3695797105378167D-01, &
    -.9353634542809267D+00,-.6312274766498699D+00, &
    0.9353634542809267D+00,0.6312274766498701D+00, &
    -.9674061994537118D-01,-.1150463641736545D+00, &
    0.9674061994537116D-01,0.1150463641736545D+00, &
    -.9438572336357420D+00,-.3747271550737232D+00, &
    0.9438572336357420D+00,0.3747271550737233D+00, &
    -.7904714944143627D+00,0.5008869009053424D+00, &
    0.7904714944143627D+00,-.5008869009053424D+00, &
    -.5513908795637696D+00,0.1608699923234913D+00, &
    0.5513908795637696D+00,-.1608699923234912D+00, &
    -.8210097847827826D+00,-.5657668007893579D+00, &
    0.8210097847827826D+00,0.5657668007893579D+00, &
    -.8970730060299638D+00,0.7712590259177344D+00, &
    0.8970730060299638D+00,-.7712590259177344D+00, &
    -.9113544664316420D+00,0.4163780011554896D+00, &
    0.9113544664316420D+00,-.4163780011554895D+00, &
    -.5579917371493338D+00,-.5836842069804584D+00, &
    0.5579917371493338D+00,0.5836842069804584D+00, &
    -.8254893061838994D+00,-.2215359986069006D+00, &
    0.8254893061838994D+00,0.2215359986069007D+00, &
    -.6957123574117627D+00,-.4009765829120038D+00, &
    0.6957123574117627D+00,0.4009765829120039D+00, &
    -.3763993540852892D+00,-.2343918950839542D-01, &
    0.3763993540852892D+00,0.2343918950839547D-01, &
    -.7179044391714098D+00,-.7105079759089011D+00, &
    0.7179044391714098D+00,0.7105079759089011D+00, &
    -.5431094815533654D+00,-.2200751569931638D+00, &
    0.5431094815533654D+00,0.2200751569931639D+00 /
  data ws / &
    0.1227297430951934D-02,0.1227297430951934D-02, &
    0.1227297430951934D-02,0.1227297430951934D-02, &
    0.2705540778783016D-02,0.2705540778783016D-02, &
    0.2705540778783016D-02,0.2705540778783016D-02, &
    0.1737911403412906D-02,0.1737911403412906D-02, &
    0.1737911403412906D-02,0.1737911403412906D-02, &
    0.4392077971620324D-02,0.4392077971620324D-02, &
    0.4392077971620324D-02,0.4392077971620324D-02, &
    0.5022826333532844D-02,0.5022826333532844D-02, &
    0.5022826333532844D-02,0.5022826333532844D-02, &
    0.7874070819611865D-02,0.7874070819611865D-02, &
    0.7874070819611865D-02,0.7874070819611865D-02, &
    0.4436852998533855D-02,0.4436852998533855D-02, &
    0.4436852998533855D-02,0.4436852998533855D-02, &
    0.5267024709906290D-02,0.5267024709906290D-02, &
    0.5267024709906290D-02,0.5267024709906290D-02, &
    0.5674802470601283D-02,0.5674802470601283D-02, &
    0.5674802470601283D-02,0.5674802470601283D-02, &
    0.1214093209674204D-01,0.1214093209674204D-01, &
    0.1214093209674204D-01,0.1214093209674204D-01, &
    0.6621349971001042D-02,0.6621349971001042D-02, &
    0.6621349971001042D-02,0.6621349971001042D-02, &
    0.1231662091800690D-01,0.1231662091800690D-01, &
    0.1231662091800690D-01,0.1231662091800690D-01, &
    0.3418869012187221D-02,0.3418869012187221D-02, &
    0.3418869012187221D-02,0.3418869012187221D-02, &
    0.1030951784492058D-01,0.1030951784492058D-01, &
    0.1030951784492058D-01,0.1030951784492058D-01, &
    0.7362609227691093D-02,0.7362609227691093D-02, &
    0.7362609227691093D-02,0.7362609227691093D-02, &
    0.1132084139272881D-01,0.1132084139272881D-01, &
    0.1132084139272881D-01,0.1132084139272881D-01, &
    0.1579946795763282D-01,0.1579946795763282D-01, &
    0.1579946795763282D-01,0.1579946795763282D-01, &
    0.1762701971434555D-01,0.1762701971434555D-01, &
    0.1762701971434555D-01,0.1762701971434555D-01, &
    0.1445134560321702D-01,0.1445134560321702D-01, &
    0.1445134560321702D-01,0.1445134560321702D-01, &
    0.4104428749850738D-01,0.4104428749850738D-01, &
    0.4104428749850738D-01,0.4104428749850738D-01, &
    0.3339495242559154D-01,0.3339495242559154D-01, &
    0.3339495242559154D-01,0.3339495242559154D-01, &
    0.4184373496097664D-01,0.4184373496097664D-01, &
    0.4184373496097664D-01,0.4184373496097664D-01, &
    0.2495764669470555D-01,0.2495764669470555D-01, &
    0.2495764669470555D-01,0.2495764669470555D-01, &
    0.3707130727927116D-01,0.3707130727927116D-01, &
    0.3707130727927116D-01,0.3707130727927116D-01, &
    0.9609787849027714D-02,0.9609787849027714D-02, &
    0.9609787849027714D-02,0.9609787849027714D-02, &
    0.3355691943165765D-01,0.3355691943165765D-01, &
    0.3355691943165765D-01,0.3355691943165765D-01, &
    0.8580982368089280D-02,0.8580982368089280D-02, &
    0.8580982368089280D-02,0.8580982368089280D-02, &
    0.1755951049274731D-01,0.1755951049274731D-01, &
    0.1755951049274731D-01,0.1755951049274731D-01, &
    0.4298498004935582D-01,0.4298498004935582D-01, &
    0.4298498004935582D-01,0.4298498004935582D-01, &
    0.2200937149220828D-01,0.2200937149220828D-01, &
    0.2200937149220828D-01,0.2200937149220828D-01, &
    0.1430848314758049D-01,0.1430848314758049D-01, &
    0.1430848314758049D-01,0.1430848314758049D-01, &
    0.1667506010717100D-01,0.1667506010717100D-01, &
    0.1667506010717100D-01,0.1667506010717100D-01, &
    0.3444495645764802D-01,0.3444495645764802D-01, &
    0.3444495645764802D-01,0.3444495645764802D-01, &
    0.2897820038333119D-01,0.2897820038333119D-01, &
    0.2897820038333119D-01,0.2897820038333119D-01, &
    0.3492622185053053D-01,0.3492622185053053D-01, &
    0.3492622185053053D-01,0.3492622185053053D-01, &
    0.4881429507850928D-01,0.4881429507850928D-01, &
    0.4881429507850928D-01,0.4881429507850928D-01, &
    0.2338133304387865D-01,0.2338133304387865D-01, &
    0.2338133304387865D-01,0.2338133304387865D-01, &
    0.4325777192033273D-01,0.4325777192033273D-01, &
    0.4325777192033273D-01,0.4325777192033273D-01 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
  w(1:n) = ws(1:n)

  return
end
subroutine rule30 ( n, x, w )

!*****************************************************************************80
!
!! RULE30 returns the rule of degree 30.
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
  parameter ( ns = 167 )

  real ( kind = 8 ) w(n)
  real ( kind = 8 ) ws(ns)
  real ( kind = 8 ) x(2,n)
  real ( kind = 8 ) xs(ns)
  real ( kind = 8 ) ys(ns)

  save xs
  save ys
  save ws

  data xs / &
    -.2489884108477549D+00,-.8955668996881347D+00, &
    -.9323001501704753D+00,0.7405445449992548D+00, &
    -.9340642229281805D+00,-.9095962664223526D+00, &
    0.5608633663495270D+00,0.8616511147917938D+00, &
    0.1660321315312064D+00,0.6574874238191415D+00, &
    -.1433381379425067D+00,0.1329569336990351D+00, &
    -.8829748195890315D+00,0.6632673829575200D+00, &
    0.4936163200514849D+00,-.2027212042708638D+00, &
    -.9958809661720823D+00,-.3073287548883581D+00, &
    -.8217303027626981D+00,0.8261783389660409D+00, &
    -.4397988250817157D+00,0.5010528086431736D+00, &
    0.5909729749119785D+00,-.3540551267912417D+00, &
    -.1341504650225714D+00,0.8797782958287345D+00, &
    0.7856276574168712D+00,0.2511140307013671D-01, &
    0.6793669279007186D+00,0.5990617334262452D+00, &
    -.6571411220532614D-01,0.2486338305340984D+00, &
    0.2517441361617084D+00,0.4956264496869945D+00, &
    0.6407808309272243D+00,0.3783356039497430D+00, &
    0.4110110511714637D+00,-.8395840683102380D+00, &
    -.8086877648916926D+00,0.1348481347528898D+00, &
    0.7911211318084024D-01,0.1744159174802842D+00, &
    0.3212567387892802D-01,-.9336794010864112D-02, &
    0.7322797884447179D+00,0.3357167967967313D-01, &
    0.5935826720491633D+00,0.8968431432339444D+00, &
    0.1799598313156058D+00,0.7269821230572000D+00, &
    -.1856842034490502D+00,0.6913593560408486D+00, &
    -.4397815672742683D+00,-.7758135419229234D+00, &
    -.9616538724444206D+00,0.3193596929830660D+00, &
    0.3777165903330749D+00,-.7679662364686681D+00, &
    -.7298701218806570D+00,0.4065079920371972D+00, &
    -.9227465688784468D-01,-.5950684639841661D+00, &
    -.9013518967720980D+00,0.8836909793143869D+00, &
    -.9967771339953647D+00,-.9944392441466685D+00, &
    -.2195956599465894D+00,-.4351118739522196D+00, &
    -.4349934895265168D+00,0.8129226389660974D+00, &
    0.9133061964092235D+00,-.7096773902564911D+00, &
    -.7019587221008244D-01,0.3087832511180271D+00, &
    -.4541723274189980D+00,0.1380191907138354D+00, &
    0.9551957279557286D+00,-.7407772548705425D+00, &
    -.6078105587102869D+00,-.8435179784664049D+00, &
    0.5490559994565902D+00,0.2444037338918357D+00, &
    0.2400810439649010D+00,-.5106820540745040D-01, &
    0.5284630091276127D+00,0.8294520284860720D+00, &
    0.9644131173686892D+00,0.5513347774529967D+00, &
    -.7997415011464921D+00,0.9063983320825396D-01, &
    0.5014531965594803D+00,-.3847230717105717D+00, &
    -.2781131563704615D+00,-.9344492367354887D+00, &
    -.9030917852729532D+00,-.4172241922741589D-01, &
    -.2640772153341865D+00,0.9665539194330073D+00, &
    -.2367623099741220D+00,-.8626350293444497D+00, &
    -.5924624188363723D+00,-.2560781746707642D+00, &
    -.9697346473592514D+00,-.5129714977732606D+00, &
    0.9006954577534996D+00,0.4485570029784398D+00, &
    -.9929762013100525D+00,-.4556636018911340D+00, &
    -.6294980126555781D+00,-.3125079163601901D+00, &
    -.7343713176297997D+00,-.6080741370996756D+00, &
    0.7969083548584857D+00,0.9691866547839980D+00, &
    0.3151361183804022D+00,0.3344055950815164D+00, &
    0.9472309841785602D+00,-.1247403647674209D+00, &
    -.9779732163470612D+00,-.7256364179379601D+00, &
    0.9970921358526692D+00,-.9954560187756241D+00, &
    0.9951087682452733D+00,-.6883166208655062D+00, &
    0.4290000273640588D+00,-.9880739319478465D+00, &
    0.9072764372199305D+00,0.9928546893888752D+00, &
    -.9917918025218534D+00,-.4805464361132344D+00, &
    -.8469642292616028D+00,-.8369560183233818D+00, &
    -.9779640479084950D+00,0.9687313192733549D+00, &
    -.7547772532654357D+00,0.7252641746474808D+00, &
    0.6646022965583286D+00,-.5644091993909820D+00, &
    0.8026432404348592D+00,0.9543957566085060D+00, &
    -.3557861147500951D+00,0.9951864328005721D+00, &
    -.9294765518337259D+00,-.9726603739446762D+00, &
    0.9908893344007184D+00,0.9593076671548199D+00, &
    -.9203911103942219D+00,-.5020036235062807D+00, &
    0.8473616010099635D+00,0.7880730531161636D+00, &
    0.1506248287699496D+00,0.9937769310041399D+00, &
    0.9894968458978942D+00,0.5755438442317007D+00, &
    0.9058856705910572D+00,0.9189816123793266D+00, &
    0.6951883785315084D+00,-.6234868878005112D+00, &
    -.9575663408305919D+00,-.9618986262036308D+00, &
    0.8232277101227666D+00,0.9715176930569319D+00, &
    -.6414127133627360D+00,-.9547454138171275D+00, &
    -.9923199594879274D+00,-.8900231674458331D+00, &
    0.9908836025186869D+00 /
  data ys / &
    -.2460485077113066D+00,-.4965476117162992D+00, &
    -.6487707118457230D+00,0.3697665250429530D+00, &
    -.2243181000931358D+00,-.1272532317878749D+00, &
    0.6675821699023886D-01,-.3572065887354983D+00, &
    -.5859511801028319D+00,0.2705477662356954D+00, &
    -.1384585540894461D+00,-.4984429846834076D+00, &
    -.6385136736297631D+00,-.9972347998858430D+00, &
    0.9914800246598282D+00,0.7647568592516673D+00, &
    0.5617037705569573D+00,0.7101121364821149D+00, &
    -.9361722473508081D+00,0.4495309095930795D+00, &
    0.9980930920696178D+00,0.1901925696164289D+00, &
    0.9914568702475882D+00,-.3600355152827119D+00, &
    0.5732016635520147D+00,-.4984298059079483D+00, &
    -.2335476149195165D+00,0.6688792604932079D+00, &
    -.5195288082893980D-01,-.2801670391820981D+00, &
    0.8352476010432909D+00,-.7081859480565704D+00, &
    -.8979072023199524D+00,-.4437687184357049D+00, &
    -.5837384948634179D+00,0.3614240616611519D+00, &
    -.8230684981188165D+00,0.3172654777326074D-01, &
    -.9623528941703298D+00,0.7321217620214555D+00, &
    -.8096610148202545D+00,-.1586165019412888D+00, &
    0.2323785604104418D-01,-.3309606689238450D+00, &
    -.4388893082509347D+00,0.4126373875717694D+00, &
    0.7615965342755699D+00,0.2500590258181676D+00, &
    0.2336722577585314D+00,0.8396217552527814D+00, &
    -.5038771822936119D+00,0.5974085228316085D+00, &
    0.9608827453877556D+00,0.9958546191278792D+00, &
    -.4499429575894099D+00,0.5431833943398334D-01, &
    -.5768346162092601D+00,0.9424496317962137D+00, &
    0.2149562696494443D+00,-.9626198942478345D+00, &
    -.9006091929700322D+00,0.3935553572852833D+00, &
    -.9903441091784717D+00,-.9730346543555619D-01, &
    0.2076974492755955D+00,-.6015458100234288D+00, &
    -.7914402846211577D+00,-.9950700583024972D+00, &
    0.5506915379662010D+00,0.6946004094913842D+00, &
    0.5611276703738952D+00,-.9902901583614324D+00, &
    -.9923523090719414D+00,-.3380059761467528D+00, &
    0.2166811227803833D+00,0.9084360793204729D+00, &
    -.6099079065118447D+00,-.1613429998348757D+00, &
    0.2829088743783339D-01,-.3475035436069812D+00, &
    0.4843880673732963D+00,-.9945285505783277D+00, &
    0.5463996157581324D+00,0.9658858240647616D+00, &
    -.7106923954489180D+00,0.9126926988703539D+00, &
    0.9995360204936274D+00,-.9899747511065149D+00, &
    -.7645030064887491D+00,-.9580271636184000D+00, &
    0.8948233603717312D+00,-.8974768652542269D+00, &
    0.3866240780739671D+00,0.7812537795763034D+00, &
    -.8567508357551102D+00,-.6608100052801699D+00, &
    0.9104312794921889D+00,0.3794498701460580D+00, &
    0.9917517335327534D+00,0.8711471531031385D+00, &
    0.6865345494324926D+00,-.9676402895588128D+00, &
    -.7614862881147397D+00,-.7974426670548660D+00, &
    0.8007048860105550D+00,-.1342520685119810D+00, &
    -.2726893919561137D+00,0.8259904245622020D+00, &
    -.3535762642754187D+00,0.2570082752527717D-01, &
    0.5408483080025682D+00,0.9034516527900712D+00, &
    0.1094396192245524D+00,-.9898880600690686D+00, &
    0.8171118504095591D+00,0.9624825870234074D+00, &
    -.3058427697709690D+00,0.2042047511467314D+00, &
    0.6881014856247000D+00,0.8038659949683351D+00, &
    -.9483358470837810D+00,-.8774593970142823D+00, &
    0.8159614789170281D+00,-.8758831247154076D+00, &
    0.6649096212014286D+00,-.9861910838287823D+00, &
    0.9701981255999362D+00,-.7285580220078556D+00, &
    0.9697520026151714D+00,-.1654759673552738D+00, &
    0.3730458721291450D+00,0.6880375541948958D+00, &
    -.1463794215119850D-01,0.6931515112743655D+00, &
    -.5310794389210227D+00,-.9586758167636964D+00, &
    0.9527988373282164D+00,-.9549818827769936D+00, &
    -.6636898088045536D+00,0.4709151069275523D-01, &
    -.6618488641031038D+00,0.5367182637758624D+00, &
    0.1898490002080080D+00,0.3931950072226694D+00, &
    0.9627353314786278D+00,0.8959541550605267D+00, &
    0.5524608659159009D+00,-.5236758593851664D+00, &
    -.9898656273130529D+00,0.9909061133703810D+00, &
    0.9951465511067116D+00,0.1849675400643637D+00, &
    -.1455654267961535D+00,-.9037901015984503D+00, &
    -.7708757339972588D+00,-.9444760769791583D+00, &
    -.7980610860639374D+00,0.9837596096190924D+00, &
    -.9414770920443709D+00,0.9976903799315803D+00, &
    -.8814229711931103D+00,-.8637004475570209D+00, &
    -.6773031038124658D+00,0.9224881482845090D+00, &
    0.8455113672684367D+00,0.9760969611534657D+00, &
    -.4540561623942587D+00 /
  data ws / &
    0.1656982041847443D-01,0.6241957476859499D-02, &
    0.5216414751791839D-02,0.1560407651121931D-01, &
    0.9922352722380101D-02,0.1044885486074365D-01, &
    0.1879260280925844D-01,0.7828025997082013D-02, &
    0.6646815879562722D-02,0.2535391408286021D-01, &
    0.3596968831636211D-01,0.3381660954529117D-01, &
    0.1200214214883407D-01,0.1160844683478245D-02, &
    0.3455740863097736D-02,0.1153404181974989D-01, &
    0.2326794602666712D-02,0.2497552316469688D-01, &
    0.6829794315843321D-02,0.1834140231596609D-01, &
    0.1335225445575319D-02,0.2522798332446686D-01, &
    0.1595934032110033D-02,0.3325015027182070D-01, &
    0.3467011496498743D-01,0.1611839351364714D-01, &
    0.2222188812186740D-01,0.1902350040458492D-01, &
    0.2953427938744245D-01,0.2950256499869300D-01, &
    0.2375703035217899D-01,0.2641584105673240D-01, &
    0.1690793382324671D-01,0.2648926396425241D-01, &
    0.2312689012710705D-01,0.3659471133442508D-01, &
    0.2037361988036312D-01,0.2428599858933432D-01, &
    0.2744225331234096D-02,0.2314679166528731D-01, &
    0.2431857706237597D-01,0.3987956826883379D-01, &
    0.4134270745058791D-01,0.4048978066628536D-01, &
    0.2251078653857245D-01,0.4125065697406070D-01, &
    0.2175419992100145D-01,0.1864439716615348D-01, &
    0.3968876145073878D-01,0.1521710713143089D-01, &
    0.3736662827386808D-01,0.2360382881992710D-01, &
    0.9970157722257745D-02,0.1786030245435301D-02, &
    0.1077371157065800D-01,0.3789581847120879D-01, &
    0.2650233252873021D-01,0.8678377005773784D-02, &
    0.3088113765197352D-01,0.1042906365431807D-01, &
    0.1836452487305106D-01,0.3403335573613761D-01, &
    0.2495937103585967D-02,0.1987508148017507D-01, &
    0.3001373057974097D-02,0.3414083443427366D-02, &
    0.2598635525971543D-01,0.3548865418196895D-02, &
    0.3429487078025957D-01,0.1785504393755270D-01, &
    0.1456023929593576D-01,0.3984363472067331D-02, &
    0.4363711971919699D-02,0.3712059926790805D-01, &
    0.3986478887569449D-01,0.1913617153849744D-01, &
    0.1049511173281814D-01,0.3060958199220223D-01, &
    0.3647607908842043D-01,0.2174227606463140D-01, &
    0.3117137400117779D-01,0.3796109966676785D-02, &
    0.3772008807242438D-01,0.1190095135082278D-01, &
    0.2275866415131214D-01,0.9507317753716060D-02, &
    0.6602435439732473D-03,0.3562338138346624D-02, &
    0.1790794872114957D-01,0.1166720090620105D-01, &
    0.1835513327961018D-01,0.1782419810205321D-01, &
    0.4039879332467389D-01,0.8544698960003014D-02, &
    0.1019822565974266D-01,0.3318239678979050D-01, &
    0.1843639498443942D-01,0.1049436999441544D-01, &
    0.4401594721967702D-02,0.1035186584479928D-01, &
    0.2774663878371582D-01,0.1038891259079699D-01, &
    0.7200083574761216D-02,0.2284618397210542D-01, &
    0.1143761979472999D-01,0.3626034532564225D-01, &
    0.4552809166608951D-02,0.2292163519800657D-01, &
    0.3339012139070554D-01,0.4305115781657949D-01, &
    0.2689701371924873D-01,0.1397250038349306D-01, &
    0.2657597849484922D-01,0.1419885923832320D-02, &
    0.2482810620945660D-01,0.1189134969439302D-01, &
    0.1376786292971261D-01,0.4412810694947523D-01, &
    0.5396384182228113D-02,0.1824530377035380D-01, &
    0.1045375639904588D-02,0.1889471014408118D-02, &
    0.2161857112903824D-02,0.1668110451401195D-01, &
    0.3023692617299402D-01,0.9232726779228300D-03, &
    0.4418205190510442D-02,0.3242974435474479D-02, &
    0.1103547225797253D-02,0.3921202767387279D-01, &
    0.2351260224707112D-01,0.1749878242380525D-01, &
    0.9478556031843440D-02,0.7823816828409005D-02, &
    0.2512073023506692D-01,0.8748252477568070D-02, &
    0.1020629440191134D-01,0.1142386634483088D-01, &
    0.2036353481009834D-01,0.1287466988683176D-01, &
    0.3056687396551737D-01,0.3145655833062538D-02, &
    0.1700461827659660D-01,0.9197009770615088D-02, &
    0.1441218157540074D-02,0.5481672105012487D-02, &
    0.1455840676178983D-01,0.3167421637373095D-01, &
    0.3035466841344861D-02,0.3337345728148663D-02, &
    0.4142211764571325D-02,0.4237586226174837D-02, &
    0.5970530714448440D-02,0.1594194686963999D-01, &
    0.1264147552673667D-01,0.5857851156194439D-02, &
    0.1967714628271757D-01,0.5157040758112765D-02, &
    0.4236825269003221D-02,0.7120803451144430D-03, &
    0.1238091173770127D-01,0.5529561908151683D-02, &
    0.2467962146273505D-01,0.4839010235595776D-02, &
    0.2559138088025228D-02,0.4161994856862944D-02, &
    0.4970131407881912D-02 /

  call r8mat_row_copy ( 2, n, 1, xs, x )
  call r8mat_row_copy ( 2, n, 2, ys, x )
  w(1:n) = ws(1:n)

  return
end
subroutine square_arbq ( degree, n, x, w )

!*****************************************************************************80
!
!! SQUARE_ARBQ returns a quadrature rule for the symmetric square.
!
!  Discussion:
!
!    This procedure returns a quadrature rule for smooth functions
!    on the unit square [-1,1]^2.
!
!    All quadratures are accurate to 15 digits
!    All weights are positive and inside the square
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
!    This can be determined by a call to SQUARE_ARBQ_SIZE(DEGREE).
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
  else if ( degree == 21 ) then
    call rule21 ( n, x, w )
  else if ( degree == 22 ) then
    call rule22 ( n, x, w )
  else if ( degree == 23 ) then
    call rule23 ( n, x, w )
  else if ( degree == 24 ) then
    call rule24 ( n, x, w )
  else if ( degree == 25 ) then
    call rule25 ( n, x, w )
  else if ( degree == 26 ) then
    call rule26 ( n, x, w )
  else if ( degree == 27 ) then
    call rule27 ( n, x, w )
  else if ( degree == 28 ) then
    call rule28 ( n, x, w )
  else if ( degree == 29 ) then
    call rule29 ( n, x, w )
  else if ( degree == 30 ) then
    call rule30 ( n, x, w )
  else
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'SQUARE_ARBQ - Fatal error!'
    write ( *, '(a)' ) '  Illegal value of DEGREE.'
    stop 1
  end if

  w_sum = sum ( w(1:n) )

  w(1:n) = 4.0D+00 * w(1:n) / w_sum

  return
end
subroutine square_arbq_gnuplot ( n, x, header )

!*****************************************************************************80
!
!! SQUARE_ARBQ_GNUPLOT: GNUPLOT plot a quadrature rule for the symmetric square.
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
subroutine square_arbq_size ( degree, n )

!*****************************************************************************80
!
!! SQUARE_ARBQ_SIZE: size of quadrature rule for the symmetric square.
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
!    1 <= DEGREE <= 30.
!
!    Output, integer ( kind = 4 ) N, the number of points in the
!    corresponding rule.
!
  implicit none

  integer ( kind = 4 ) degree
  integer ( kind = 4 ) n
  integer ( kind = 4 ) n_save(30)

  save n_save

  data n_save / &
      1,   3,   4,   6,   7, &
     10,  12,  16,  17,  22, &
     24,  31,  33,  41,  44, &
     52,  55,  64,  68,  78, &
     82,  93,  98, 109, 115, &
    127, 132, 147, 152, 167 /

  if ( degree .lt. 1 .or. 30 .lt. degree ) then
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'SQUARE_ARBQ_SIZE - Fatal error!'
    write ( *, '(a)' ) '  Illegal value of DEGREE.'
    stop 1
  end if

  n = n_save ( degree )

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
