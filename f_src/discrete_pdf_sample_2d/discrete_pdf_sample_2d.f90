program main

!*****************************************************************************80
!
!! MAIN is the main program for DISCRETE_PDF_SAMPLE_2D.
!
!  Discussion:
!
!    This program is an example of how discrete sample or density data
!    can be used to define a PDF (probability density function). 
!
!    In this function and the functions it calls, we assume that we have
!    data for an array of 20 by 20 square subcells of the unit square.
!    We wish to derive a PDF that will allow us to sample an arbitrary
!    number of points from this region.
!
!    In particular, we intend to use the discrete data to generate a PDF
!    which we will then use to generate sample points.
!
!    Roughly speaking, we have kept track of how many fish we caught in
!    each part of a lake, and now we want to simulate catching N fish
!    under the same conditions.
!
!    The statistics for each simulation should be governed by the discrete
!    PDF, but with random variation.  In other words, the actual number
!    of points taken from each subregion is random, and the actual location of
!    each point in a subregion is random, but over many simulations, the
!    statistics of the sample points should reproduce the statistics of
!    the original discrete sample that defined the PDF.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 December 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Local, integer ( kind = 4 ) N, the number of sample points to be generated.
!
  implicit none

  integer ( kind = 4 ) n

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DISCRETE_PDF_SAMPLE_2D:'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Generate sample data using a discrete PDF.'

  n = 1000

  call test01 ( n )
  call test02 ( n )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DISCRETE_PDF_SAMPLE_2D:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop
end
subroutine test01 ( n )

!*****************************************************************************80
!
!! TEST01 looks at a 20x20 region.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 December 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of sample points to be generated.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ), allocatable :: cdf(:,:)
  character ( len = 255 ) filename
  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  real ( kind = 8 ), allocatable :: pdf(:,:)
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable:: u(:)
  real ( kind = 8 ), allocatable :: xy(:,:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Consider data skewed toward the upper left corner'
  write ( *, '(a)' ) '  of the unit square.'
  write ( *, '(a,i4,a)' ) '  Generate ', n, ' samples'
!
!  Get the dimensions of the PDF data.
!
  call get_discrete_pdf_size1 ( n1, n2 )
  write ( *, '(a,i4,a,i4,a)' ) '  PDF data is on a ', n1, ' by ', n2, ' grid.'
!
!  Construct a PDF from the data.
!
  allocate ( pdf(1:n1,1:n2) )
  call get_discrete_pdf_data1 ( n1, n2, pdf )
!
!  "Integrate" the data over rows and columns of the region to get the CDF.
!
  allocate ( cdf(1:n1,1:n2) )
  call set_discrete_cdf ( n1, n2, pdf, cdf )
!
!  Choose N CDF values at random.
!
  seed = 123456789
  allocate ( u(1:n) )

  call r8vec_uniform_01 ( n, seed, u )
!
!  Find the cell corresponding to each CDF value,
!  and choose a random point in that cell.
!
  allocate ( xy(1:2,1:n) )

  call discrete_cdf_to_xy ( n1, n2, cdf, n, u, seed, xy )
!
!  Write data to a file for examination, plotting, or analysis.
!
  filename = 'test01.txt'
  call r8mat_write ( filename, 2, n, xy )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  Wrote sample data to file "' // trim ( filename ) // '".'
!
!  Free memory.
!
  deallocate ( cdf )
  deallocate ( pdf )
  deallocate ( u )
  deallocate ( xy )

  return
end
subroutine test02 ( n )

!*****************************************************************************80
!
!! TEST02 looks at a 12x8 region.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 December 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of sample points to be generated.
!
  implicit none

  integer ( kind = 4 ) n

  real ( kind = 8 ), allocatable :: cdf(:,:)
  character ( len = 255 ) filename
  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2
  real ( kind = 8 ), allocatable :: pdf(:,:)
  integer ( kind = 4 ) seed
  real ( kind = 8 ), allocatable:: u(:)
  real ( kind = 8 ), allocatable :: xy(:,:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST02'
  write ( *, '(a)' ) &
    '  Consider data suggested by the shape and density of Iowa.'
  write ( *, '(a,i4,a)' ) '  Generate ', n, ' samples'
!
!  Get the dimensions of the PDF data.
!
  call get_discrete_pdf_size2 ( n1, n2 )
  write ( *, '(a,i4,a,i4,a)' ) '  PDF data is on a ', n1, ' by ', n2, ' grid.'
!
!  Construct a PDF from the data.
!
  allocate ( pdf(1:n1,1:n2) )
  call get_discrete_pdf_data2 ( n1, n2, pdf )
!
!  "Integrate" the data over rows and columns of the region to get the CDF.
!
  allocate ( cdf(1:n1,1:n2) )
  call set_discrete_cdf ( n1, n2, pdf, cdf )
!
!  Choose N CDF values at random.
!
  seed = 123456789
  allocate ( u(1:n) )

  call r8vec_uniform_01 ( n, seed, u )
!
!  Find the cell corresponding to each CDF value,
!  and choose a random point in that cell.
!
  allocate ( xy(1:2,1:n) )

  call discrete_cdf_to_xy ( n1, n2, cdf, n, u, seed, xy )
!
!  Write data to a file for examination, plotting, or analysis.
!
  filename = 'test02.txt'
  call r8mat_write ( filename, 2, n, xy )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) &
    '  Wrote sample data to file "' // trim ( filename ) // '".'
!
!  Free memory.
!
  deallocate ( cdf )
  deallocate ( pdf )
  deallocate ( u )
  deallocate ( xy )

  return
end
subroutine discrete_cdf_to_xy ( n1, n2, cdf, n, u, seed, xy )

!*****************************************************************************80
!
!! DISCRETE_CDF_TO_XY finds XY points corresponding to discrete CDF values.
!
!  Discussion:
!
!    This program is given a discrete CDF function and a set of N random
!    values U.  Each value of U corresponds to a particular (I,J) subregion
!    whose CDF value just exceeds the value of U.  Inside that subregion,
!    we pick a point at random - this is equivalent to assuming the PDF
!    is constant over the subregion.
!
!    This function is part of an example program, for which various
!    assumptions have been made.  In particular, the region is the unit
!    square, and the subregions are formed by an N1 by N2 grid of rectangles.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N1, N2, the number of rows and columns
!    of PDF data.
!
!    Input, real ( kind = 8 ) CDF(N1,N2), the CDF values associated with each 
!    subcell.  A particular ordering has been given to the subcells so that the
!    CDF is a monotonoe function when the subcells are listed in that order.
!
!    Input, integer ( kind = 4 ) N, the number of sample points.
!
!    Input, real ( kind = 8 ) U(N), N random values.
!
!    Input/output, integer ( kind = 4 ) SEED, a seed for the random 
!    number generator.
!
!    Output, real ( kind = 8 ) XY(2,N), the sample points in the unit square.
!
  implicit none

  integer ( kind = 4 ) n
  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2

  real ( kind = 8 ) cdf(n1,n2)
  real ( kind = 8 ) high
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  integer ( kind = 4 ) k
  real ( kind = 8 ) low
  real ( kind = 8 ) r(2)
  integer ( kind = 4 ) seed
  real ( kind = 8 ) u(n)
  real ( kind = 8 ) xy(2,n)

  xy(1:2,1:n) = 0.0D+00

  low = 0.0D+00
  do j = 1, n2
    do i = 1, n1
      high = cdf(i,j);
      do k = 1, n
        if ( low <= u(k) .and. u(k) <= high ) then
          call r8vec_uniform_01 ( 2, seed, r )
          xy(1,k) = ( real ( i - 1, kind = 8 ) + r(1) ) / real ( n1, kind = 8 )
          xy(2,k) = ( real ( j - 1, kind = 8 ) + r(2) ) / real ( n2, kind = 8 )
        end if
      end do
      low = high
    end do
  end do

  return
end
subroutine get_discrete_pdf_data1 ( n1, n2, pdf )

!*****************************************************************************80
!
!! GET_DISCRETE_PDF_DATA1 returns the discrete PDF data array.
!
!  Discussion:
!
!    Cell (I,J) extends from 
!
!      (I-1) * H < Y < I * H
!      (J-1) * H < X < J * H
!
!    We have data for each cell, representing the integral of some PDF
!    over that cell.  The function pdf(x,y) must be nonnegative.  However,
!    we don't impose any other conditions on it.
!
!    The array PDF(:,:) contains the integral of pdf(x,y) over each cell,
!    or, almost as good, simply a sample or average value.
!
!    We load the array PDF, and then we normalize it so that the sum of
!    all the entries is 1.  
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N1, N2, the number of rows and columns
!    of PDF data.
!
!    Output, real ( kind = 8 ) PDF(N1,N2).  PDF(I,J) is the discrete PDF 
!    for the cell (I,J), normalized so that the sum over all cells is 1.
!
  implicit none

  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2

  real ( kind = 8 ) pdf(n1,n2)
  real ( kind = 8 ), save :: pdf_save(20,20) = reshape ( (/ &
    0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, &
    0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, &
    0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, &
    0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, &
    0.0000D+00, 0.0000D+00, 0.0001D+00, 0.0001D+00, 0.0002D+00, &
    0.0002D+00, 0.0002D+00, 0.0003D+00, 0.0003D+00, 0.0003D+00, &
    0.0003D+00, 0.0003D+00, 0.0002D+00, 0.0002D+00, 0.0002D+00, &
    0.0002D+00, 0.0001D+00, 0.0001D+00, 0.0001D+00, 0.0000D+00, &
    0.0000D+00, 0.0001D+00, 0.0002D+00, 0.0003D+00, 0.0004D+00, &
    0.0004D+00, 0.0005D+00, 0.0006D+00, 0.0006D+00, 0.0006D+00, &
    0.0006D+00, 0.0006D+00, 0.0005D+00, 0.0005D+00, 0.0004D+00, &
    0.0003D+00, 0.0003D+00, 0.0002D+00, 0.0001D+00, 0.0000D+00, &
    0.0000D+00, 0.0002D+00, 0.0003D+00, 0.0005D+00, 0.0006D+00, &
    0.0008D+00, 0.0009D+00, 0.0009D+00, 0.0010D+00, 0.0010D+00, &
    0.0010D+00, 0.0009D+00, 0.0008D+00, 0.0008D+00, 0.0007D+00, &
    0.0006D+00, 0.0004D+00, 0.0003D+00, 0.0002D+00, 0.0000D+00, &
    0.0000D+00, 0.0003D+00, 0.0005D+00, 0.0008D+00, 0.0010D+00, &
    0.0012D+00, 0.0014D+00, 0.0015D+00, 0.0015D+00, 0.0015D+00, &
    0.0015D+00, 0.0014D+00, 0.0013D+00, 0.0011D+00, 0.0010D+00, &
    0.0008D+00, 0.0006D+00, 0.0005D+00, 0.0003D+00, 0.0000D+00, &
    0.0000D+00, 0.0004D+00, 0.0009D+00, 0.0013D+00, 0.0016D+00, &
    0.0019D+00, 0.0021D+00, 0.0023D+00, 0.0023D+00, 0.0023D+00, &
    0.0021D+00, 0.0020D+00, 0.0018D+00, 0.0016D+00, 0.0013D+00, &
    0.0011D+00, 0.0009D+00, 0.0007D+00, 0.0004D+00, 0.0000D+00, &
    0.0000D+00, 0.0007D+00, 0.0014D+00, 0.0020D+00, 0.0025D+00, &
    0.0030D+00, 0.0033D+00, 0.0034D+00, 0.0034D+00, 0.0033D+00, &
    0.0031D+00, 0.0028D+00, 0.0025D+00, 0.0022D+00, 0.0018D+00, &
    0.0015D+00, 0.0012D+00, 0.0009D+00, 0.0006D+00, 0.0000D+00, &
    0.0000D+00, 0.0011D+00, 0.0021D+00, 0.0031D+00, 0.0039D+00, &
    0.0045D+00, 0.0049D+00, 0.0051D+00, 0.0050D+00, 0.0047D+00, &
    0.0043D+00, 0.0039D+00, 0.0034D+00, 0.0029D+00, 0.0024D+00, &
    0.0019D+00, 0.0015D+00, 0.0011D+00, 0.0007D+00, 0.0000D+00, &
    0.0000D+00, 0.0017D+00, 0.0033D+00, 0.0048D+00, 0.0060D+00, &
    0.0069D+00, 0.0074D+00, 0.0074D+00, 0.0072D+00, 0.0066D+00, &
    0.0059D+00, 0.0052D+00, 0.0045D+00, 0.0037D+00, 0.0031D+00, &
    0.0025D+00, 0.0019D+00, 0.0014D+00, 0.0009D+00, 0.0000D+00, &
    0.0000D+00, 0.0025D+00, 0.0050D+00, 0.0073D+00, 0.0091D+00, &
    0.0104D+00, 0.0109D+00, 0.0107D+00, 0.0101D+00, 0.0091D+00, &
    0.0080D+00, 0.0068D+00, 0.0057D+00, 0.0047D+00, 0.0038D+00, &
    0.0030D+00, 0.0023D+00, 0.0017D+00, 0.0011D+00, 0.0000D+00, &
    0.0000D+00, 0.0038D+00, 0.0075D+00, 0.0110D+00, 0.0136D+00, &
    0.0153D+00, 0.0157D+00, 0.0151D+00, 0.0138D+00, 0.0121D+00, &
    0.0104D+00, 0.0087D+00, 0.0071D+00, 0.0058D+00, 0.0046D+00, &
    0.0036D+00, 0.0027D+00, 0.0019D+00, 0.0012D+00, 0.0000D+00, &
    0.0000D+00, 0.0055D+00, 0.0110D+00, 0.0160D+00, 0.0198D+00, &
    0.0218D+00, 0.0219D+00, 0.0205D+00, 0.0182D+00, 0.0155D+00, &
    0.0129D+00, 0.0106D+00, 0.0085D+00, 0.0068D+00, 0.0053D+00, &
    0.0041D+00, 0.0031D+00, 0.0022D+00, 0.0014D+00, 0.0000D+00, &
    0.0000D+00, 0.0077D+00, 0.0154D+00, 0.0224D+00, 0.0276D+00, &
    0.0299D+00, 0.0293D+00, 0.0266D+00, 0.0229D+00, 0.0190D+00, &
    0.0154D+00, 0.0123D+00, 0.0098D+00, 0.0077D+00, 0.0059D+00, &
    0.0045D+00, 0.0034D+00, 0.0024D+00, 0.0015D+00, 0.0000D+00, &
    0.0000D+00, 0.0100D+00, 0.0202D+00, 0.0295D+00, 0.0362D+00, &
    0.0385D+00, 0.0368D+00, 0.0324D+00, 0.0271D+00, 0.0219D+00, &
    0.0174D+00, 0.0137D+00, 0.0107D+00, 0.0082D+00, 0.0063D+00, &
    0.0048D+00, 0.0035D+00, 0.0025D+00, 0.0016D+00, 0.0000D+00, &
    0.0000D+00, 0.0120D+00, 0.0244D+00, 0.0356D+00, 0.0432D+00, &
    0.0455D+00, 0.0426D+00, 0.0366D+00, 0.0298D+00, 0.0236D+00, &
    0.0184D+00, 0.0143D+00, 0.0110D+00, 0.0084D+00, 0.0064D+00, &
    0.0048D+00, 0.0035D+00, 0.0025D+00, 0.0016D+00, 0.0000D+00, &
    0.0000D+00, 0.0134D+00, 0.0266D+00, 0.0382D+00, 0.0461D+00, &
    0.0480D+00, 0.0445D+00, 0.0376D+00, 0.0301D+00, 0.0235D+00, &
    0.0181D+00, 0.0139D+00, 0.0106D+00, 0.0081D+00, 0.0061D+00, &
    0.0046D+00, 0.0033D+00, 0.0023D+00, 0.0015D+00, 0.0000D+00, &
    0.0000D+00, 0.0151D+00, 0.0261D+00, 0.0362D+00, 0.0436D+00, &
    0.0447D+00, 0.0412D+00, 0.0347D+00, 0.0276D+00, 0.0214D+00, &
    0.0164D+00, 0.0125D+00, 0.0095D+00, 0.0072D+00, 0.0054D+00, &
    0.0041D+00, 0.0029D+00, 0.0021D+00, 0.0013D+00, 0.0000D+00, &
    0.0000D+00, 0.0174D+00, 0.0220D+00, 0.0295D+00, 0.0349D+00, &
    0.0361D+00, 0.0333D+00, 0.0281D+00, 0.0225D+00, 0.0175D+00, &
    0.0134D+00, 0.0102D+00, 0.0078D+00, 0.0059D+00, 0.0044D+00, &
    0.0033D+00, 0.0024D+00, 0.0017D+00, 0.0010D+00, 0.0000D+00, &
    0.0000D+00, 0.0097D+00, 0.0152D+00, 0.0200D+00, 0.0235D+00, &
    0.0244D+00, 0.0227D+00, 0.0193D+00, 0.0156D+00, 0.0122D+00, &
    0.0094D+00, 0.0072D+00, 0.0055D+00, 0.0041D+00, 0.0031D+00, &
    0.0023D+00, 0.0017D+00, 0.0012D+00, 0.0007D+00, 0.0000D+00, &
    0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, &
    0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, &
    0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, &
    0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00, 0.0000D+00 /), &
    (/ 20, 20 /) )
  real ( kind = 8 ) total

  pdf(1:n1,1:n2) = pdf_save(1:20,1:20)
!
!  Normalize to get an integral of 1.
!
  total = sum ( pdf(1:n1,1:n2) )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  PDF data sums to ', total

  pdf(1:n1,1:n2) = pdf(1:n1,1:n2) / total

  return
end
subroutine get_discrete_pdf_data2 ( n1, n2, pdf )

!*****************************************************************************80
!
!! GET_DISCRETE_PDF_DATA2 returns the discrete PDF data array.
!
!  Discussion:
!
!    Cell (I,J) extends from 
!
!      (I-1) * H < Y < I * H
!      (J-1) * H < X < J * H
!
!    We have data for each cell, representing the integral of some PDF
!    over that cell.  The function pdf(x,y) must be nonnegative.  However,
!    we don't impose any other conditions on it.
!
!    The array PDF(:,:) contains the integral of pdf(x,y) over each cell,
!    or, almost as good, simply a sample or average value.
!
!    We load the array PDF, and then we normalize it so that the sum of
!    all the entries is 1.  
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 December 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N1, N2, the number of rows and columns
!    of PDF data.
!
!    Output, real ( kind = 8 ) PDF(N1,N2).  PDF(I,J) is the discrete PDF 
!    for the cell (I,J), normalized so that the sum over all cells is 1.
!
  implicit none

  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2

  real ( kind = 8 ) pdf(n1,n2)
  real ( kind = 8 ), save :: pdf_save(12,8) = reshape ( (/ &
     10.0,  20.0,  10.0,  10.0,  20.0,  10.0, &
     30.0,  10.0,  10.0,  10.0,  10.0,  50.0, &
     25.0,  30.0,  10.0,  25.0,  30.0,  40.0, &
     30.0,  20.0,  10.0,  20.0,  30.0,  40.0, &
     25.0,  30.0,  20.0,  10.0,  40.0, 200.0, &
     50.0,  40.0,  10.0,  30.0,  60.0,  40.0, &
     20.0,  30.0,  40.0,  10.0,  75.0, 100.0, &
    100.0,  30.0,  25.0,  25.0,  90.0,  30.0, &
     75.0,  15.0,  20.0,  10.0,  75.0,  50.0, &
     40.0,  10.0, 100.0,  25.0,  25.0,  80.0, &
     25.0,  50.0,  50.0,  10.0,  25.0,  25.0, &
     15.0,  10.0,  25.0,  25.0,  10.0,  10.0, &
    100.0,  50.0,  50.0,  10.0,  10.0,  10.0, &
      5.0, 100.0,  50.0,  50.0,  10.0,  10.0, &
     10.0,  10.0,  25.0,  50.0,  10.0,  50.0, & 
     10.0,  50.0,  25.0,  25.0,  25.0,  10.0 /), &
    (/ 12, 8 /) )
  real ( kind = 8 ) total

  pdf(1:n1,1:n2) = pdf_save(1:12,1:8)
!
!  Normalize to get an integral of 1.
!
  total = sum ( pdf(1:n1,1:n2) )

  write ( *, '(a)' ) ' '
  write ( *, '(a,g14.6)' ) '  PDF data sums to ', total

  pdf(1:n1,1:n2) = pdf(1:n1,1:n2) / total

  return
end
subroutine get_discrete_pdf_size1 ( n1, n2 )

!*****************************************************************************80
!
!! GET_DISCRETE_PDF_SIZE1 returns the dimension of the discrete PDF data
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 December 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) N1, N2, the number of rows and columns
!    of data.
!
  implicit none

  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2

  n1 = 20
  n2 = 20

  return
end
subroutine get_discrete_pdf_size2 ( n1, n2 )

!*****************************************************************************80
!
!! GET_DISCRETE_PDF_SIZE2 returns the dimension of the discrete PDF data
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 December 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer ( kind = 4 ) N1, N2, the number of rows and columns
!    of data.
!
  implicit none

  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2

  n1 = 12
  n2 = 8

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
  logical lopen

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
subroutine r8mat_write ( output_filename, m, n, table )

!*****************************************************************************80
!
!! R8MAT_WRITE writes an R8MAT file.
!
!  Discussion:
!
!    An R8MAT is an array of R8 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) OUTPUT_FILENAME, the output file name.
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, integer ( kind = 4 ) N, the number of points.
!
!    Input, real ( kind = 8 ) TABLE(M,N), the data.
!
  implicit none

  integer ( kind = 4 ) m
  integer ( kind = 4 ) n

  integer ( kind = 4 ) j
  character ( len = * ) output_filename
  integer ( kind = 4 ) output_status
  integer ( kind = 4 ) output_unit
  character ( len = 30 ) string
  real ( kind = 8 ) table(m,n)
!
!  Open the file.
!
  call get_unit ( output_unit )

  open ( unit = output_unit, file = output_filename, &
    status = 'replace', iostat = output_status )

  if ( output_status /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8MAT_WRITE - Fatal error!'
    write ( *, '(a,i8)' ) '  Could not open the output file "' // &
      trim ( output_filename ) // '" on unit ', output_unit
    output_unit = -1
    stop
  end if
!
!  Create a format string.
!
!  For less precision in the output file, try:
!
!                                            '(', m, 'g', 14, '.', 6, ')'
!
  if ( 0 < m .and. 0 < n ) then

    write ( string, '(a1,i8,a1,i8,a1,i8,a1)' ) '(', m, 'g', 24, '.', 16, ')'
!
!  Write the data.
!
    do j = 1, n
      write ( output_unit, string ) table(1:m,j)
    end do

  end if
!
!  Close the file.
!
  close ( unit = output_unit )

  return
end
subroutine r8vec_uniform_01 ( n, seed, r )

!*****************************************************************************80
!
!! R8VEC_UNIFORM_01 returns a unit pseudorandom R8VEC.
!
!  Discussion:
!
!    An R8VEC is a vector of R8's.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2006
!
!  Author:
!
!    John Burkardt
!
!  Reference:
!
!    Paul Bratley, Bennett Fox, Linus Schrage,
!    A Guide to Simulation,
!    Springer Verlag, pages 201-202, 1983.
!
!    Bennett Fox,
!    Algorithm 647:
!    Implementation and Relative Efficiency of Quasirandom
!    Sequence Generators,
!    ACM Transactions on Mathematical Software,
!    Volume 12, Number 4, pages 362-376, 1986.
!
!    Peter Lewis, Allen Goodman, James Miller
!    A Pseudo-Random Number Generator for the System/360,
!    IBM Systems Journal,
!    Volume 8, pages 136-143, 1969.
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N, the number of entries in the vector.
!
!    Input/output, integer ( kind = 4 ) SEED, the "seed" value, which
!    should NOT be 0.  On output, SEED has been updated.
!
!    Output, real ( kind = 8 ) R(N), the vector of pseudorandom values.
!
  implicit none

  integer ( kind = 4 ) n

  integer ( kind = 4 ) i
  integer ( kind = 4 ) k
  integer ( kind = 4 ) seed
  real ( kind = 8 ) r(n)

  if ( seed == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8VEC_UNIFORM_01 - Fatal error!'
    write ( *, '(a)' ) '  Input value of SEED = 0.'
    stop
  end if

  do i = 1, n

    k = seed / 127773

    seed = 16807 * ( seed - k * 127773 ) - k * 2836

    if ( seed < 0 ) then
      seed = seed + 2147483647
    end if

    r(i) = real ( seed, kind = 8 ) * 4.656612875D-10

  end do

  return
end
subroutine set_discrete_cdf ( n1, n2, pdf, cdf )

!*****************************************************************************80
!
!! SET_DISCRETE_CDF sets a CDF from a discrete PDF.
!
!  Discussion:
!
!    Here, we proceed from cell (1,1) to (2,1) to 
!    (N1,1), (1,2), (2,2)...(N1,N2).
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    03 January 2012
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer ( kind = 4 ) N1, N2, the number of rows and columns
!    of PDF data.
!
!    Input, real ( kind = 8 ) PDF(N1,N2), the discrete PDF for the cell (I,J),
!    normalized so that the sum over all cells is 1.
!
!    Output, real ( kind = 8 ) CDF(N1,N2), the discrete CDF for the cell (I,J).
!    CDF(N1,N2) should be 1.
!
  implicit none

  integer ( kind = 4 ) n1
  integer ( kind = 4 ) n2

  real ( kind = 8 ) cdf(n1,n2)
  integer ( kind = 4 ) i
  integer ( kind = 4 ) j
  real ( kind = 8 ) pdf(n1,n2)
  real ( kind = 8 ) total

  total = 0.0D+00
  do j = 1, n2
    do i = 1, n1
      total = total + pdf(i,j)
      cdf(i,j) = total
    end do
  end do

  return
end
subroutine s_to_i4 ( s, value, ierror, length )

!*****************************************************************************80
!
!! S_TO_I4 reads an integer value from a string.
!
!  Discussion:
!
!    Instead of ICHAR, we now use the IACHAR function, which
!    guarantees the ASCII collating sequence.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 January 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) S, a string to be examined.
!
!    Output, integer ( kind = 4 ) VALUE, the integer value read from the string.
!    If the string is blank, then VALUE will be returned 0.
!
!    Output, integer ( kind = 4 ) IERROR, an error flag.
!    0, no error.
!    1, an error occurred.
!
!    Output, integer ( kind = 4 ) LENGTH, the number of characters
!    of S used to make the integer.
!
  implicit none

  character c
  integer ( kind = 4 ) i
  integer ( kind = 4 ) ierror
  integer ( kind = 4 ) isgn
  integer ( kind = 4 ) length
  character ( len = * ) s
  integer ( kind = 4 ) state
  character :: TAB = achar ( 9 )
  integer ( kind = 4 ) value

  value = 0
  ierror = 0
  length = 0

  state = 0
  isgn = 1

  do i = 1, len_trim ( s )

    c = s(i:i)
!
!  STATE = 0, haven't read anything.
!
    if ( state == 0 ) then

      if ( c == ' ' .or. c == TAB ) then

      else if ( c == '-' ) then
        state = 1
        isgn = -1
      else if ( c == '+' ) then
        state = 1
        isgn = +1
      else if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
        state = 2
        value = iachar ( c ) - iachar ( '0' )
      else
        ierror = 1
        return
      end if
!
!  STATE = 1, have read the sign, expecting digits or spaces.
!
    else if ( state == 1 ) then

      if ( c == ' ' .or. c == TAB ) then

      else if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then
        state = 2
        value = iachar ( c ) - iachar ( '0' )
      else
        ierror = 1
        return
      end if
!
!  STATE = 2, have read at least one digit, expecting more.
!
    else if ( state == 2 ) then

      if ( lle ( '0', c ) .and. lle ( c, '9' ) ) then

        value = 10 * value + iachar ( c ) - iachar ( '0' )

      else

        value = isgn * value
        ierror = 0
        length = i - 1
        return

      end if

    end if

  end do
!
!  If we read all the characters in the string, see if we're OK.
!
  if ( state == 2 ) then

    value = isgn * value
    ierror = 0
    length = len_trim ( s )

  else

    value = 0
    ierror = 1
    length = 0

  end if

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
