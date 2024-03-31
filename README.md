# Fortran 90 codes of John Burkardt by category

* [Cluster Analysis](#cluster-analysis)
* [Combinatorics](#combinatorics)
* [Computational Geometry](#computational-geometry)
* [Eigenvalues](#eigenvalues)
* [Fast Fourier Transform and other transforms](#fast-fourier-transform-and-other-transforms)
* [Fortran tools](#fortran-tools)
* [Graph algorithms](#graph-algorithms)
* [Interpolation and Approximation](#interpolation-and-approximation)
* [Linear Algebra](#linear-algebra)
* [Linear Algebra (Sparse)](#linear-algebra-sparse)
* [Linear Equations](#linear-equations)
* [Nonlinear Equations and Nonlinear Least Squares](#nonlinear-equations-and-nonlinear-least-squares)
* [Non-Uniform Random Number Generation](#non-uniform-random-number-generation)
* [Numerical Methods](#numerical-methods)
* [Optimization -- 1D](#Optimization-1D)
* [Optimization](#Optimization)
* [Ordinary Differential Equations](#ordinary-differential-equations)
* [Probability Distributions](#probability-distributions)
* [Root-Finding](#root-finding)
* [Sorting](#sorting)
* [Special functions](#special-functions)
* [Statistics](#statistics)
* [Quadrature](#quadrature)
* [Quadrature-N-Dimensional](#quadrature-n-dimensional)
* [Quasirandom Number Generation](#quasirandom-number-generation)
* [Uniform Random Number Generation](#uniform-random-number-generation)
* [Wavelet Transforms](#wavelet-transforms)

## Cluster Analysis

[asa058](https://people.sc.fsu.edu/~jburkardt/f_src/asa058/asa058.html) handles the K-Means problem, by David Sparks.

[asa113](https://people.sc.fsu.edu/~jburkardt/f_src/asa113/asa113.html) divides M points in N dimensions into K clusters seeking the division which minimizes a user-defined criterion, by Banfield and Bassill.

[cities](https://people.sc.fsu.edu/~jburkardt/f_src/cities/cities.html) works with problems involving intercity distances.

[gene_cluster](https://people.sc.fsu.edu/~jburkardt/f_src/gene_cluster/gene_cluster.html) divides a set of genetic data into clusters.

[kmeans](https://people.sc.fsu.edu/~jburkardt/f_src/kmeans/kmeans.html) handles the K-Means problem, which organizes a set of N points in M dimensions into K clusters

[spaeth](https://people.math.sc.edu/Burkardt/f_src/spaeth/spaeth.html) analyzes data by grouping it into clusters.

## Combinatorics

[change_making](https://people.sc.fsu.edu/~jburkardt/f_src/change_making/change_making.html) considers the change making problem, in which a given sum is to be formed using coins of various denominations.

[combo](https://people.sc.fsu.edu/~jburkardt/f_src/combo/combo.html) routines for ranking, unranking, enumerating and randomly selecting balanced sequences, cycles, graphs, Gray codes, subsets, partitions, permutations, restricted growth functions, Pruefer codes and trees.

[knapsack_01](https://people.math.sc.edu/Burkardt/f_src/knapsack_01/knapsack_01.html) uses brute force to solve small versions of the 0/1 knapsack problem.

[lau_np](https://people.sc.fsu.edu/~jburkardt/f_src/lau_np/lau_np.html) implements heuristic algorithms for certain "hard" problems, by Hang Tong Lau.

[partial_digest](https://people.math.sc.edu/Burkardt/f_src/partial_digest/partial_digest.html) recursive solutions of the partial digest problem.

[partition_problem](https://people.sc.fsu.edu/~jburkardt/f_src/partition_problem/partition_problem.html) seeks solutions of the partition problem, splitting a set of integers into two subsets with equal sum.

[set_theory](https://people.math.sc.edu/Burkardt/f_src/set_theory/set_theory.html): implements some of the operations of set theory.

[subset](https://people.sc.fsu.edu/~jburkardt/f_src/subset/subset.html) enumerates, generates, randomizes, ranks and unranks combinatorial objects including combinations, compositions, Gray codes, index sets, partitions, permutations, polynomials, subsets, and Young tables.

[subset_sum](https://people.math.sc.edu/Burkardt/f_src/subset_sum/subset_sum.html) seeks solutions of the subset sum problem.

[toms515](https://people.math.sc.edu/Burkardt/f_src/toms515/toms515.html) lists the subsets of size K selected from a set of size N, by Bill Buckles, Matthew Lybanon.

[tsp_brute](https://people.sc.fsu.edu/~jburkardt/f_src/tsp_brute/tsp_brute.html) solves small versions of the traveling salesman problem, using brute force.

[tsp_lau](https://people.sc.fsu.edu/~jburkardt/f_src/tsp_lau/tsp_lau.html) implements a heuristic algorithm for solution of the traveling salesman problem, by Hang Tong Lau.

## Computational Geometry

[cvt](https://people.sc.fsu.edu/~jburkardt/f_src/cvt/cvt.html): computes Centroidal Voronoi Tessellation (CVT) datasets.

[cvt_basis](https://people.sc.fsu.edu/~jburkardt/f_src/cvt_basis/cvt_basis.html): uses discrete Centroidal Voronoi Tessellation (CVT) techniques to produce a small set of basis vectors that are good cluster centers for a large set of data vectors.

[cvt_basis_flow](https://people.sc.fsu.edu/~jburkardt/f_src/cvt_basis_flow/cvt_basis_flow.html): extracts representative solution modes of a set of solutions to a fluid flow PDE. The selection process uses K-Means clustering, which can be considered to be a discrete version of the CVT algorithm (Centroidal Voronoi Tessellation).

[cvtp](https://people.sc.fsu.edu/~jburkardt/f_src/cvtp/cvtp.html): creates CVTP's, that is, Centroidal Voronoi Tessellations of a periodic region.

[cvt_triangulation](https://people.math.sc.edu/Burkardt/f_src/cvt_triangulation/cvt_triangulation.html): applies Centroidal Voronoi Tessellation (CVT) methods to produce triangularizations of various test regions.

[delaunay_lmap_2d](https://people.math.sc.edu/Burkardt/f_src/delaunay_lmap_2d/delaunay_lmap_2d.html): computes the Delaunay triangulation of a set of points in the plane that have been transformed by a linear map A.

[dutch](https://people.sc.fsu.edu/~jburkardt/f_src/dutch/dutch.html): implements some computational geometry routines.

[geometry](https://people.sc.fsu.edu/~jburkardt/f_src/geometry/geometry.html): carries out geometric calculations, including angles, areas, containment, distances, intersections, lengths, and volumes.

[geompack2](https://people.math.sc.edu/Burkardt/f_src/geompack2/geompack2.html): which carries out certain geometric computations, including the Voronoi diagram, and the Delaunay triangulation of a set of points in the plane, by Barry Joe.

[geompack3](https://people.math.sc.edu/Burkardt/f_src/geompack3/geompack3.html): handles certain computational geometry problems, by Barry Joe. In particular, GEOMPACK3 can compute the Voronoi diagram, and the Delaunay triangulation, of a set of points in the plane, and can carry out analogous operations for points in 3D and in N-dimensional space.

[mesh_bandwidth](https://people.math.sc.edu/Burkardt/f_src/mesh_bandwidth/mesh_bandwidth.html): computes the geometric bandwidth of a mesh.

[sphere_delaunay](https://people.sc.fsu.edu/~jburkardt/f_src/sphere_delaunay/sphere_delaunay.html): computes the Delaunay triangulation of points on the unit sphere.

[stripack](https://people.math.sc.edu/Burkardt/f_src/stripack/stripack.html): carries out some computational geometry tasks on the unit sphere in 3D, by Robert Renka.

[table_delaunay](https://people.math.sc.edu/Burkardt/f_src/table_delaunay/table_delaunay.html): computes the Delaunay triangulation of a set of points in the plane.

[triangulation](https://people.math.sc.edu/Burkardt/f_src/triangulation/triangulation.html): library which computes a triangulation of a set of points in 2D, and carries out various other related operations on triangulations of order 3 or 6.

[triangulation_boundary_nodes](https://people.math.sc.edu/Burkardt/f_src/triangulation_boundary_nodes/triangulation_boundary_nodes.html): analyzes the triangulation of a region, and identifies each boundary node with the label "1".

[triangulation_corner](https://people.math.sc.edu/Burkardt/f_src/triangulation_corner/triangulation_corner.html): tries to correct situations in which a triangulation includes corner triangles, that is, triangles which have two sides on boundary.

[triangulation_delaunay_discrepancy](https://people.math.sc.edu/Burkardt/f_src/triangulation_delaunay_discrepancy/triangulation_delaunay_discrepancy.html): computes the local Delaunay discrepancy of a triangulation.

[triangulation_histogram](https://people.math.sc.edu/Burkardt/f_src/triangulation_histogram/triangulation_histogram.html): computes the number of points from a dataset that are contained in each triangle of a triangulation.

[triangulation_l2q](https://people.math.sc.edu/Burkardt/f_src/triangulation_l2q/triangulation_l2q.html): reads information describing a triangulation of a set of points using 3-node ("linear") triangles, and creates a 6-node ("quadratic") triangulation.

[triangulation_mask](https://people.math.sc.edu/Burkardt/f_src/triangulation_mask/triangulation_mask.html): which reads the nodes and triangles that define a triangulation, calls a user routine which determines whether each triangle is to be preserved or discarded ("masked") from the triangulation, and writes out new node and triangle files that define the masked triangulation.

[triangulation_node_to_element](https://people.math.sc.edu/Burkardt/f_src/triangulation_node_to_element/triangulation_node_to_element.html): reads datafiles describing a set of nodes, their triangulation, and the value of one or more quantities at each node, and outputs a file that averages the quantities for each element.

[triangulation_orient](https://people.math.sc.edu/Burkardt/f_src/triangulation_orient/triangulation_orient.html): reads a triangulation, and reorients each triangle that has a negative area. If at least one such triangle is encountered, the program writes out a new copy of the triangle file in which all the triangles have been correctly oriented.

[triangulation_plot](https://people.math.sc.edu/Burkardt/f_src/triangulation_plot/triangulation_plot.html): reads one file listing the nodes, and a second file consisting of groups of 3 or 6 nodes that make up triangles, and creates an Encapsulated PostScript image of the triangulation.

[triangulation_q2l](https://people.math.sc.edu/Burkardt/f_src/triangulation_q2l/triangulation_q2l.html): reads information describing a triangulation of a set of points using 6-node ("quadratic") triangles, and creates a 3-node ("linear") triangulation.

[triangulation_quad](https://people.math.sc.edu/Burkardt/f_src/triangulation_quad/triangulation_quad.html): reads information defining a triangulation, and estimates the integral of a function whose values are given at the nodes.

[triangulation_quality](https://people.math.sc.edu/Burkardt/f_src/triangulation_quality/triangulation_quality.html): computes and prints quality measures for a given triangulation of a set of points in 2D.

[triangulation_rcm](https://people.math.sc.edu/Burkardt/f_src/triangulation_rcm/triangulation_rcm.html): computes the Reverse Cuthill-McKee (RCM) reordering for nodes in a triangulation composed of 3-node or 6-node triangles.

[triangulation_refine](https://people.math.sc.edu/Burkardt/f_src/triangulation_refine/triangulation_refine.html): reads information describing a triangulation of a set of points and creates a refined triangulation.

[triangulation_triangle_neighbors](https://people.math.sc.edu/Burkardt/f_src/triangulation_triangle_neighbors/triangulation_triangle_neighbors.html): computes the three neighboring triangles of each triangle in a triangulation.

## Eigenvalues 

[arpack](https://people.math.sc.edu/Burkardt/f_src/arpack/arpack.html) computes eigenvalues and eigenvectors of large matrices, by Richard Lehoucq, Danny Sorensen, Chao Yang.

[eispack](https://people.sc.fsu.edu/~jburkardt/f77_src/eispack/eispack.html) calculates the eigenvalues and eigenvectors of a matrix.

[jacobi_eigenvalue](https://people.math.sc.edu/Burkardt/f_src/jacobi_eigenvalue/jacobi_eigenvalue.html) computes the eigenvalues and eigenvectors of a real symmetric matrix.

[power_method](https://people.math.sc.edu/Burkardt/f_src/power_method/power_method.html) Power Method for Eigenvalues and Eigenvectors.

[test_eigen](https://people.math.sc.edu/Burkardt/f_src/test_eigen/test_eigen.html) Test Matrices for Eigenvalue Analysis.

[toms384](https://people.math.sc.edu/Burkardt/f77_src/toms384/toms384.html) implements ACM TOMS algorithm 384, for computing the eigenvalues and eigenvectors of a symmetric matrix.

## Fast Fourier Transform and other Transforms
[cosine_transform](https://people.sc.fsu.edu/~jburkardt/f_src/cosine_transform/cosine_transform.html) demonstrates some simple properties of the discrete cosine transform (DCT) for real data.

[fftpack5](https://people.sc.fsu.edu/~jburkardt/f_src/fftpack5/fftpack5.html) computes Fast Fourier Transforms, by Paul Swarztrauber and Dick Valent. Real or complex data can be handled, there are separate routines for forward analysis (data => Fourier coefficients) and backward analysis (Fourier coefficients => data), there are sine and cosine transform routines, there are quarter wave sine and cosine transform routines, and the amount of data is NOT required to be a power of 2.

[fftpack51](https://people.sc.fsu.edu/~jburkardt/f_src/fftpack51/fftpack51.html) is a double precision version of fftpack5. 

[fftw_test](https://people.sc.fsu.edu/~jburkardt/f_src/fftw_test/fftw_test.html) calls [fftw()](https://www.fftw.org/), which computes fast Fourier transforms, written by Matteo Frigo and Steven Johnson.

[sftpack](https://people.sc.fsu.edu/~jburkardt/f_src/sftpack/sftpack.html) implements the slow Fourier transform, intended as a teaching tool and comparison with the Fast Fourier Transform (FFT).

[sine_transform](https://people.sc.fsu.edu/~jburkardt/f_src/sine_transform/sine_transform.html) demonstrates some simple properties of the discrete sine transform for real data.

## Fortran Tools
[extract](https://people.math.sc.edu/Burkardt/f_src/f90split/f90split.html) extracts a subroutine, function or module by name from a FORTRAN file.

[f77_to_f90](https://people.math.sc.edu/Burkardt/f_src/f77_to_f90/f77_to_f90.html) converts fixed source form to free source form.

[fixcon](https://people.math.sc.edu/Burkardt/f_src/fixcon/fixcon.html) converts the line continuation scheme used in a FORTRAN77 file to that used in FORTRAN90 files.

[include_files](https://people.math.sc.edu/Burkardt/f_src/f90split/f90split.html) makes a copy of a FORTRAN file, in which INCLUDE statements are replaced by the corresponding include files.

[xerror](https://people.math.sc.edu/Burkardt/f_src/xerror/xerror.html) collects information about errors that occur during a program's execution, and takes the appropriate action, which may include printing a message, diverting to a user routine, or aborting execution of the program.

## Graph Algorithms

[bellman_ford](https://people.sc.fsu.edu/~jburkardt/f_src/bellman_ford/bellman_ford.html) implements the Bellman-Ford algorithm for finding the shortest distance from a given node to all other nodes in a directed graph whose edges have been assigned real-valued lengths.

[codepack](https://people.math.sc.edu/Burkardt/f_src/codepack/codepack.html): computes and compares "codes" for graphs, directed graphs, multigraphs, and other generalizations of an abstract graph.

[djikstra](https://people.sc.fsu.edu/~jburkardt/f_src/dijkstra/dijkstra.html) implements a simple version of Dijkstra's algorithm for determining the minimum distance from one node in a graph to all other nodes.

[floyd](https://people.math.sc.edu/Burkardt/f_src/floyd/floyd.html): implements Floyd's algorithm for finding the shortest distance between every pair of nodes in a directed graph.

[grafpack](https://people.sc.fsu.edu/~jburkardt/f_src/grafpack/grafpack.html) performs common calculations involving (abstract mathematical) graphs.

[laupack](https://people.sc.fsu.edu/~jburkardt/f_src/laupack/laupack.html) computes properties of mathematical graphs, including Euler circuits, Hamiltonian circuits, cliques, strongly connected components, minimal spanning tree, chromatic number, shortest paths, maximal flow, and planarity.

[toms097](https://people.math.sc.edu/Burkardt/f_src/toms097/toms097.html) computes the distance between all pairs of nodes in a directed graph with weighted edges, using Floyd's algorithm.

[treepack](https://people.math.sc.edu/Burkardt/f_src/treepack/treepack.html) performs common calculations involving a special kind of graph known as a tree.

## Graphics

[dislin_test](https://people.sc.fsu.edu/~jburkardt/f_src/dislin_test/dislin_test.html) illustrates the use of the DISLIN scientific plotting package.

[graphics_dislin_test](https://people.sc.fsu.edu/~jburkardt/f_src/graphics_dislin_test/graphics_dislin_test.html) illustrates how various kinds of data can be displayed and analyzed graphically, using the dislin() graphics package.

[graphics_gnuplot_test](https://people.sc.fsu.edu/~jburkardt/f_src/graphics_gnuplot_test/graphics_gnuplot_test.html) illustrate how various kinds of data can be displayed and analyzed graphically, using the gnuplot() graphics package.

[gnufor](https://people.math.sc.edu/Burkardt/f_src/gnufor/gnufor.html) interface to the GNUPLOT plotting software.

## Interpolation and Approximation

[barycentric_interp_1d](https://people.sc.fsu.edu/~jburkardt/f_src/barycentric_interp_1d/barycentric_interp_1d.html) defines and evaluates the Lagrange polynomial p(x) which interpolates a set of data, so that p(x(i)) = y(i).

[bivar](https://people.sc.fsu.edu/~jburkardt/f_src/bivar/bivar.html) interpolates scattered bivariate data, by Hiroshi Akima.

[chebyshev](https://people.math.sc.edu/Burkardt/f_src/chebyshev/chebyshev.html) constructs the Chebyshev interpolant to a function.

[chebyshev_interp_1d](https://people.math.sc.edu/Burkardt/f_src/chebyshev_interp_1d/chebyshev_interp_1d.html) determines the combination of Chebyshev polynomials which interpolates a set of data, so that p(x(i)) = y(i).

[chebyshev_series](https://people.math.sc.edu/Burkardt/f_src/chebyshev_series/chebyshev_series.html) evaluate a Chebyshev series approximating a function f(x), while efficiently computing one, two or three derivatives of the series, which approximate f'(x), f''(x), and f'''(x), by Manfred Zimmer.

[divdif](https://people.math.sc.edu/Burkardt/f_src/divdif/divdif.html) creates, prints and manipulates divided difference polynomials based on data tabulated at evenly spaced or unevenly spaced argument values.

[hermite](https://people.math.sc.edu/Burkardt/f_src/hermite/hermite.html) constructs the Hermite polynomial which interpolates function and derivative values at given points.

[interp](https://people.math.sc.edu/Burkardt/f_src/interp/interp.html) takes a set of data associated with successive values of a parameter, and produces an interpolating function which can be evaluated over a continuous range of the parameter.

[lagrange_interp_1d](https://people.math.sc.edu/Burkardt/f_src/lagrange_interp_1d/lagrange_interp_1d.html) defines and evaluates the Lagrange polynomial p(x) which interpolates a set of data, so that p(x(i)) = y(i).

[lagrange_interp_2d](https://people.math.sc.edu/Burkardt/f_src/lagrange_interp_2d/lagrange_interp_2d.html) defines and evaluates the Lagrange polynomial p(x,y) which interpolates a set of data depending on a 2D argument that was evaluated on a product grid, so that p(x(i),y(j)) = z(i,j).

[lagrange_interp_nd](https://people.math.sc.edu/Burkardt/f_src/lagrange_interp_nd/lagrange_interp_nd.html) defines and evaluates the Lagrange polynomial p(x) which interpolates a set of data depending on a M-dimensional argument that was evaluated on a product grid, so that p(x(i)) = z(i).

[lagrange_nd](https://people.math.sc.edu/Burkardt/f_src/lagrange_nd/lagrange_nd.html) given a set of ND points X(*) in D-dimensional space, constructs a family of ND Lagrange polynomials P(*)(X), associating polynomial P(i) with point X(i), such that, for 1 <= i <= ND, P(i)(X(i)) = 1
but, if i =/= j P(i)(X(j)) = 0

[nearest_interp_1d](https://people.math.sc.edu/Burkardt/f_src/nearest_interp_1d/nearest_interp_1d.html) interpolates a set of data using a piecewise constant interpolant defined by the nearest neighbor criterion, creating graphics files for processing by GNUPLOT.

[newton_interp_1d](https://people.sc.fsu.edu/~jburkardt/f_src/newton_interp_1d/newton_interp_1d.html) finds a polynomial interpolant to data using Newton divided differences.

[pwl_approx_1d](https://people.sc.fsu.edu/~jburkardt/f_src/pwl_approx_1d/pwl_approx_1d.html) defines and evaluates a piecewise linear function, using NC "control points", which approximates a set of ND data points (x(i),y(i)).

[pwl_interp_1d](https://people.sc.fsu.edu/~jburkardt/f_src/pwl_interp_1d/pwl_interp_1d.html) interpolates a set of data with a piecewise linear function.

[pwl_interp_2d](https://people.sc.fsu.edu/~jburkardt/f_src/pwl_interp_2d/pwl_interp_2d.html) evaluates a piecewise linear interpolant of data depending on a 2D argument, defined on on a product grid, so that p(x(i),y(j)) = z(i,j).

[pwl_interp_2d_scattered](https://people.sc.fsu.edu/~jburkardt/f_src/pwl_interp_2d_scattered/pwl_interp_2d_scattered.html) produces a piecewise linear interpolant to 2D scattered data, that is, data that is not guaranteed to lie on a regular grid.

[rbf_interp_1d](https://people.sc.fsu.edu/~jburkardt/f_src/rbf_interp_1d/rbf_interp_1d.html) defines and evaluates radial basis function (RBF) interpolants to 1D data.

[rbf_interp_2d](https://people.sc.fsu.edu/~jburkardt/f_src/rbf_interp_2d/rbf_interp_2d.html) defines and evaluates radial basis function (RBF) interpolants to 2D data.

[rbf_interp_nd](https://people.sc.fsu.edu/~jburkardt/f_src/rbf_interp_nd/rbf_interp_nd.html) defines and evaluates radial basis function (RBF) interpolants to multidimensional data.

[sparse_interp_nd](https://people.math.sc.edu/Burkardt/f_src/sparse_interp_nd/sparse_interp_nd.html) construct a sparse interpolant to a function f(x) of a multidimensional argument x.

[shephard_interp_nd](https://people.sc.fsu.edu/~jburkardt/f_src/shepard_interp_nd/shepard_interp_nd.html) defines and evaluates Shepard interpolants to multidimensional data, based on inverse distance weighting.

[test_interp_nd](https://people.math.sc.edu/Burkardt/f_src/test_interp_nd/test_interp_nd.html) provides test functions for multidimensional interpolation.

[toms446](https://people.math.sc.edu/Burkardt/f_src/toms446/toms446.html) implements ACM TOMS algorithm 446, for the manipulation of Chebyshev series, by Roger Broucke.

[toms526](https://people.math.sc.edu/Burkardt/f_src/toms526/toms526.html) interpolates scattered bivariate data, by Hiroshi Akima.

[toms660](https://people.math.sc.edu/Burkardt/f_src/toms660/toms660.html) interpolates scattered 2D data, also called "qshep2d", by Robert Renka.

[toms661](https://people.math.sc.edu/Burkardt/f_src/toms661/toms661.html) interpolates scattered 3D data, also known as "qshep3d", by Robert Renka.

[toms790](https://people.sc.fsu.edu/~jburkardt/f_src/bivar/bivar.html) constructs an interpolant to scattered 2D data, by Robert Renka.

[toms886](https://people.math.sc.edu/Burkardt/f_src/toms886/toms886.html) implements an interpolation procedure based on "Padua points", defined in the square [-1,+1]^2, whose interpolating power is especially good.

## Linear Algebra

[blas](https://people.math.sc.edu/Burkardt/f_src/blas/blas.html) Basic Linear Algebra Subprograms.

[condition](https://people.sc.fsu.edu/~jburkardt/f_src/condition/condition.html) computes or estimates the condition number of a matrix.

[hankel_cholesky](https://people.sc.fsu.edu/~jburkardt/f_src/hankel_cholesky/hankel_cholesky.html) compute the upper Cholesky factor of a positive definite symmetric (PDS) Hankel matrix H, that is, H = R' * R.

[hankel_pds](https://people.math.sc.edu/Burkardt/f_src/hankel_pds/hankel_pds.html) computes a lower triangular matrix L which is the Cholesky factor of a positive definite (symmetric) Hankel matrix H, that is, H = L * L'.

[lapack_d](https://people.sc.fsu.edu/~jburkardt/f_src/lapack_d/lapack_d.html) routines for solving systems of simultaneous linear equations, least-squares solutions of linear systems of equations, eigenvalue problems, and singular value problems.

[matrix_exponential](https://people.math.sc.edu/Burkardt/f_src/matrix_exponential/matrix_exponential.html) compares some algorithms for approximating the matrix exponential function.

[r8ge](https://people.sc.fsu.edu/~jburkardt/f_src/r8ge/r8ge.html) basic linear algebra operations on R8GE matrices (real 64 bit precision general MxN format). There is [r8lt](https://people.sc.fsu.edu/~jburkardt/f_src/r8lt/r8lt.html) for lower triangular and [r83](https://people.sc.fsu.edu/~jburkardt/f_src/r83/r83.html) for tridiagonal matrices.

[sparsekit](https://people.math.sc.edu/Burkardt/f_src/sparsekit/sparsekit.html) carries out a number of operations on sparse matrices, particularly conversion between various sparse formats.

[toeplitz_cholesky](https://people.sc.fsu.edu/~jburkardt/f_src/toeplitz_cholesky/toeplitz_cholesky.html) computes the Cholesky factorization of a positive semidefinite symmetric (PSS) Toeplitz matrix.

## Linear Algebra (Sparse)

[cc_io](https://people.math.sc.edu/Burkardt/f_src/cc_io/cc_io.html) reads and writes compressed column (CC) sparse matrix files.

[cc_to_st](https://people.math.sc.edu/Burkardt/f_src/cc_to_st/cc_to_st.html) converts sparse matrix information from compressed column (CC) to sparse triplet (ST) format.

[hb_io](https://people.math.sc.edu/Burkardt/f_src/hb_io/hb_io.html) reads and writes files in the Harwell Boeing sparse matrix format.

[hb_to_st](https://people.math.sc.edu/Burkardt/f_src/hb_to_st/hb_to_st.html) reads the definition of a sparse matrix from a file in Harwell-Boeing format, and writes the information into a Sparse Triplet file.

[mm_io](https://people.math.sc.edu/Burkardt/f_src/mm_io/mm_io.html) reads and writes files in the Matrix Market (MM) format.

[st_io](https://people.math.sc.edu/Burkardt/f_src/st_io/st_io.html): reads and writes files in the Sparse Triplet (ST) format used for storing sparse matrices.

## Linear Equations

[bvls](https://people.sc.fsu.edu/~jburkardt/f_src/bvls/bvls.html) solves a least squares problem with bounds on the variables, by Charles Lawson and Richard Hanson.

[cg](https://people.sc.fsu.edu/~jburkardt/f_src/cg/cg.html) simple version of the conjugate gradient (CG) method for solving a system of linear equations of the form A*x=b, suitable for situations in which the matrix A is positive definite (only real, positive eigenvalues) and symmetric.

[cg_rc](https://people.sc.fsu.edu/~jburkardt/f_src/cg_rc/cg_rc.html) conjugate gradient (CG) method for solving a positive definite sparse linear system A*x=b, using reverse communication (RC).

[dlap](https://people.math.sc.edu/Burkardt/f_src/dlap/dlap.html) routines for solving sparse linear systems, by Anne Greenbaum and Mark Seager.

[dlap_io](https://people.math.sc.edu/Burkardt/f_src/dlap_io/dlap_io.html): reads and writes files in the DLAP sparse matrix format.

[jacobi](https://people.sc.fsu.edu/~jburkardt/f_src/jacobi/jacobi.html) Jacobi iteration for linear systems.

[lawson](https://people.math.sc.edu/Burkardt/f77_src/lawson/lawson.html) solve least squares problems.

[linpack](https://people.math.sc.edu/Burkardt/f_src/linpack/linpack.html) analyze and solve linear equations and linear least-squares problems.

[mgmres](https://people.sc.fsu.edu/~jburkardt/f_src/mgmres/mgmres.html) restarted Generalized Minimum Residual (GMRES) algorithm to solve a sparse linear system, using compressed row (CR) or sparse triplet (ST) format, by Lili Ju.

[qr_solve](https://people.sc.fsu.edu/~jburkardt/f_src/qr_solve/qr_solve.html) computes a linear least squares (LLS) solution of a system A*x=b.

[sparsepak](https://people.math.sc.edu/Burkardt/f_src/sparsepak/sparsepak.html) old version of the Waterloo Sparse Matrix Package that solves large sparse systems of linear equations.

[superlu_test](https://people.math.sc.edu/Burkardt/f_src/superlu_test/superlu_test.html) illustrate the use of the SUPERLU library for the fast direct solution of large sparse systems of linear equations.

[templates](https://people.math.sc.edu/Burkardt/f_src/templates/templates.html) outlines the most common iterative methods of solving a linear system.

[toeplitz](https://people.sc.fsu.edu/~jburkardt/f_src/toeplitz/toeplitz.html) olves a variety of Toeplitz and circulant linear systems.

## Nonlinear Equations and Nonlinear Least Squares

[dqed](https://people.math.sc.edu/Burkardt/f_src/dqed/dqed.html) solves (square) systems of nonlinear equations, or minimizes the residual in a set of nonlinear equations, using least squares, by Richard Hanson and Fred Krogh.

[fsolve](https://people.sc.fsu.edu/~jburkardt/f_src/fsolve/fsolve.html) solves systems of nonlinear equations, inspired by the fsolve() function in MATLAB, and based on the minpack() minimization package.

[minpack](https://people.sc.fsu.edu/~jburkardt/f_src/minpack/minpack.html) solves systems of nonlinear equations, or carries out the least squares minimization of the residual of a set of linear or nonlinear equations, by Jorge More, Danny Sorenson, Burton Garbow, Kenneth Hillstrom.

[newton_rc](https://people.sc.fsu.edu/~jburkardt/f_src/newton_rc/newton_rc.html) solves a system of nonlinear equations by Newton's method, using reverse communication (RC).

[nl2sol](https://people.math.sc.edu/Burkardt/f_src/nl2sol/nl2sol.html) implements an adaptive nonlinear least-squares algorithm, by John Dennis, David Gay, Roy Welsch.

[roots_rc](https://people.math.sc.edu/Burkardt/f_src/roots_rc/roots_rc.html) seeks solutions of a system of nonlinear equations, using reverse communication (RC), by Gaston Gonnet.

## Non-uniform Random Number Generation

[asa053](https://people.sc.fsu.edu/~jburkardt/f_src/asa053/asa053.html) returns samples from the Wishart distribution.

[normal](https://people.sc.fsu.edu/~jburkardt/f_src/normal/normal.html): returns a sequence of normally distributed pseudorandom numbers.

[pdflib](https://people.sc.fsu.edu/~jburkardt/f_src/pdflib/pdflib.html) evaluates Probability Density Functions (PDF's) and produces random samples from them, including beta, binomial, chi, exponential, gamma, inverse chi, inverse gamma, multinomial, normal, scaled inverse chi, and uniform.

[random_data](https://people.sc.fsu.edu/~jburkardt/f_src/random_data/random_data.html) uses a random number generator (RNG) to sample points for various probability distributions, spatial dimensions, and geometries, including the M-dimensional cube, ellipsoid, simplex and sphere.

[ranlib](https://people.sc.fsu.edu/~jburkardt/f_src/ranlib/ranlib.html) produces random samples from Probability Density Functions (PDF's), including Beta, Chi-square Exponential, F, Gamma, Multivariate normal, Noncentral chi-square, Noncentral F, Univariate normal, random permutations, Real uniform, Binomial, Negative Binomial, Multinomial, Poisson and Integer uniform, by Barry Brown and James Lovato.

[walker_sample](https://people.sc.fsu.edu/~jburkardt/f_src/walker_sample/walker_sample.html): efficiently samples a discrete probability vector.

[wishart](https://people.sc.fsu.edu/~jburkardt/f_src/wishart/wishart.html) produces sample matrices from the Wishart or Bartlett distributions, useful for sampling random covariance matrices.

[ziggurat](https://people.sc.fsu.edu/~jburkardt/f_src/ziggurat/ziggurat.html) random number generator (RNG) for the uniform, normal or exponential distributions, by Marsaglia and Tsang.

## Numerical Methods

[nms](https://people.sc.fsu.edu/~jburkardt/f_src/nms/nms.html) accompanies the text "Numerical Methods and Software".

[slatec](https://people.math.sc.edu/Burkardt/f_src/slatec/slatec.html) general purpose mathematical and statistical routines.

## Optimization-1D

[local_min_rc](https://people.math.sc.edu/Burkardt/f_src/local_min_rc/local_min_rc.html) seeks a local minimum of a scalar function of a scalar variable, without requiring derivatives, or assuming the function is differentiable, using reverse communication (RC), by Richard Brent.

[test_min](https://people.math.sc.edu/Burkardt/f_src/test_min/test_min.html) defines problems involving the minimization of a scalar function of a scalar argument.

## Optimization

[asa047](https://people.sc.fsu.edu/~jburkardt/f_src/asa047/asa047.html) minimize a scalar function of several variables using the Nelder-Mead algorithm, by R ONeill.

[compass_search](https://people.sc.fsu.edu/~jburkardt/f_src/compass_search/compass_search.html) minimizes of a scalar function of several variables using compass search, a direct search algorithm that does not use derivatives.

[praxis](https://people.sc.fsu.edu/~jburkardt/f_src/praxis/praxis.html) minimizes a scalar function of a vector argument, without needing derivative information, by Richard Brent.

[test_opt](https://people.sc.fsu.edu/~jburkardt/f_src/test_opt/test_opt.html) defines test problems for the scalar function optimization problem.

[test_opt_con](https://people.sc.fsu.edu/~jburkardt/f_src/test_opt_con/test_opt_con.html) Test Functions for Scalar Optimization
Constrained to a Hyper-Rectangle.

[toms178](https://people.math.sc.edu/Burkardt/f_src/toms178/toms178.html) uses the Hooke-Jeeves direct search algorithm to seek the minimizing point of a function F(X) of several variables, by Arthur Kaupe.

[toms611](https://github.com/johannesgerer/jburkardt-f/tree/master/toms611) minimizes a scalar functional of multiple variables.

[uncmin](https://people.math.sc.edu/Burkardt/f77_src/uncmin/uncmin.html) seeks to minimize a scalar function of N variables.

## Ordinary Differential Equations

[etdrk4](https://people.sc.fsu.edu/~jburkardt/f_src/etdrk4/etdrk4.html) uses the ETD RK4 method to solve systems of stiff ODE's, by Aly-Khan Kassam, Lloyd Trefethen.

[euler](https://people.sc.fsu.edu/~jburkardt/f_src/euler/euler.html) solves one or more ordinary differential equations (ODEs) using the forward Euler method.

[midpoint_explicit](https://people.sc.fsu.edu/~jburkardt/f_src/midpoint_explicit/midpoint_explicit.html) solves one or more ordinary differential equations (ODE) using the (explicit) midpoint method, also known as the modified Euler method.

[ode](https://people.sc.fsu.edu/~jburkardt/f_src/ode/ode.html) solves a system of ordinary differential equations (ODE), by Shampine and Gordon.

[rk4](https://people.sc.fsu.edu/~jburkardt/f_src/rk4/rk4.html) implements a simple Runge-Kutta solver for an initial value problem.

## Probability Distributions

[asa005](https://people.sc.fsu.edu/~jburkardt/f_src/asa005/asa005.html) evaluates the lower tail of the noncentral student's T cumulative density function (CDF), by BE Cooper.

[asa066](https://people.sc.fsu.edu/~jburkardt/f_src/asa066/asa066.html) computes the cumulative density function (CDF) of the standard normal distribution, by David Hill.

[asa076](https://people.sc.fsu.edu/~jburkardt/f_src/asa076/asa076.html) evaluates Owen's T function, by Young and Minder. The function T(h, a) gives the probability of the event (X > h and 0 < Y < aX) where X and Y are independent standard normal random variables.

[asa091](https://people.sc.fsu.edu/~jburkardt/f_src/asa091/asa091.html) computes the percentage points of the Chi-Squared probability density function, by Best and Roberts.

[asa111](https://people.sc.fsu.edu/~jburkardt/f_src/asa111/asa111.html) computes the percentage points of the normal probability density function, by Beasley and Springer.

[asa152](https://people.sc.fsu.edu/~jburkardt/f_src/asa152/asa152.html) computes the cumulative probabilities associated with the hypergeometric probability distribution, by Richard Lund.

[asa226](https://people.sc.fsu.edu/~jburkardt/f_src/asa226/asa226.html) evaluates the cumulative distribution function (CDF) of the noncentral Beta Distribution, by Russell Lenth.

[asa241](https://people.sc.fsu.edu/~jburkardt/f_src/asa241/asa241.html) computes the inverse of the Normal Cumulative Density Function (CDF), by Michael Wichura.

[asa243](https://people.sc.fsu.edu/~jburkardt/f_src/asa243/asa243.html) computes the cumulative density function (CDF) of the noncentral Student's T probability density function, by Russell Lenth.

[asa266](https://people.sc.fsu.edu/~jburkardt/f_src/asa266/asa266.html) estimates the parameters of a Dirichlet probability density function (PDF).

[asa310](https://people.sc.fsu.edu/~jburkardt/f_src/asa310/asa310.html) computes the cumulative density function (CDF) of the noncentral Beta distribution, by Chattamvelli and Shanmugam.

[beta_nc](https://people.sc.fsu.edu/~jburkardt/f_src/beta_nc/beta_nc.html) evaluates the cumulative distribution function (CDF) of the noncentral Beta distribution.

[cdflib](https://people.sc.fsu.edu/~jburkardt/f_src/cdflib/cdflib.html) evaluates the cumulative density function (CDF) associated with common probability distributions, by Barry Brown, James Lovato, Kathy Russell.

[log_normal](https://people.sc.fsu.edu/~jburkardt/f_src/log_normal/log_normal.html): evaluate quantities associated with the log normal Probability Density Function (PDF).

[log_normal_truncated_ab](https://people.sc.fsu.edu/~jburkardt/f_src/log_normal_truncated_ab/log_normal_truncated_ab.html): evaluate quantities associated with the log normal Probability Density Function (PDF) truncated to the interval [A,B].

[owens](https://people.math.sc.edu/Burkardt/f_src/owens/owens.html) evaluates Owen's T function.

[prob](https://people.sc.fsu.edu/~jburkardt/f_src/prob/prob.html) routines for evaluating and inverting the normal CDF, and many other distributions.

[truncated_normal](https://people.sc.fsu.edu/~jburkardt/f_src/truncated_normal/truncated_normal.html): computes quantities associated with the truncated normal distribution.

## Quadrature
[alpert_rule](https://people.sc.fsu.edu/~jburkardt/f_src/alpert_rule/alpert_rule.html) sets up an Alpert quadrature rule for functions which are regular, log(x) singular, or 1/sqrt(x) singular.

[cauchy_principal_value](https://people.sc.fsu.edu/~jburkardt/f_src/cauchy_principal_value/cauchy_principal_value.html): uses Gauss-Legendre quadrature to estimate the Cauchy Principal Value (CPV) of certain singular integrals.

[ccn_rule](https://people.math.sc.edu/Burkardt/f_src/ccn_rule/ccn_rule.html) defines a nested Clenshaw Curtis quadrature rule.

[chebyshev1_rule](https://people.sc.fsu.edu/~jburkardt/f_src/chebyshev1_rule/chebyshev1_rule.html) computes and prints a Gauss-Chebyshev type 1 quadrature rule.

[chebyshev2_rule](https://people.sc.fsu.edu/~jburkardt/f_src/chebyshev2_rule/chebyshev2_rule.html) computes and prints a Gauss-Chebyshev type 2 quadrature rule.

[clenshaw_curtis_rule](https://people.sc.fsu.edu/~jburkardt/f_src/clenshaw_curtis_rule/clenshaw_curtis_rule.html) generates a Clenshaw Curtis quadrature rule based on user input.

[cubpack](https://people.math.sc.edu/Burkardt/f_src/cubpack/cubpack.html) estimates the integral of a function (or vector of functions) over a collection of N-dimensional hyperrectangles and simplices, by Alan Genz and Ronald Cools.

[fastgl](https://people.math.sc.edu/Burkardt/f_src/fastgl/fastgl.html) fast computation of the K-th value and weight of an N-point Gauss-Legendre quadrature rule, by Ignace Bogaert.

[filon](https://people.math.sc.edu/Burkardt/f_src/filon/filon.html) approximate integrals in which the integrand includes an oscillatory factor of sin(k*x) or cos(k*x).

[gegenbauer_rule](https://people.sc.fsu.edu/~jburkardt/f_src/gegenbauer_rule/gegenbauer_rule.html) computes and prints a Gauss-Gegenbauer quadrature rule.

[gen_hermite_rule](https://people.sc.fsu.edu/~jburkardt/f_src/gen_hermite_rule/gen_hermite_rule.html) computes and prints a generalized Gauss-Hermite quadrature rule.

[gen_laguerre_rule](https://people.sc.fsu.edu/~jburkardt/f_src/gen_laguerre_rule/gen_laguerre_rule.html) computes and prints a generalized Gauss-Laguerre quadrature rule.

[hermite_rule](https://people.sc.fsu.edu/~jburkardt/f_src/hermite_rule/hermite_rule.html) computes and prints a Gauss-Hermite quadrature rule

[intlib](https://people.math.sc.edu/Burkardt/f_src/intlib/intlib.html) contains routines for numerical estimation of integrals in 1d.

[jacobi_rule](https://people.sc.fsu.edu/~jburkardt/f_src/jacobi_rule/jacobi_rule.html) computes and prints a Gauss-Jacobi quadrature rule.

[kronrod](https://people.sc.fsu.edu/~jburkardt/f_src/kronrod/kronrod.html) computes both a Gauss quadrature rule of order N, and the Gauss-Kronrod rule of order 2*N+1.

[laguerre_rule](https://people.sc.fsu.edu/~jburkardt/f_src/laguerre_rule/laguerre_rule.html) computes and prints a Gauss-Laguerre quadrature rule.

[legendre_rule](https://people.sc.fsu.edu/~jburkardt/f_src/legendre_rule/legendre_rule.html) computes and prints a Gauss-Legendre quadrature rule.

[legendre_rule_fast](https://people.sc.fsu.edu/~jburkardt/f_src/legendre_rule_fast/legendre_rule_fast.html) uses a fast (order n) algorithm to compute a Gauss-Legendre quadrature rule of given order.

[line_integrals](https://people.math.sc.edu/Burkardt/f_src/line_integrals/line_integrals.html) returns the exact value of the integral of any monomial over the length of the unit line in 1d.

[line_ncc_rule](https://people.sc.fsu.edu/~jburkardt/f_src/line_ncc_rule/line_ncc_rule.html) computes a Newton Cotes Closed (ncc) quadrature rule for the line, that is, for an interval of the form [a,b], using equally spaced points which include the endpoints.

[line_nco_rule](https://people.sc.fsu.edu/~jburkardt/f_src/line_nco_rule/line_nco_rule.html) computes a Newton Cotes Open (nco) quadrature rule, using equally spaced points, over the interior of a line segment in 1d.

[patterson_rule](https://people.sc.fsu.edu/~jburkardt/f_src/patterson_rule/patterson_rule.html) returns the points and weights of a 1d Gauss-Patterson quadrature rule of order 1, 3, 7, 15, 31, 63, 127, 255 or 511.

[patterson_rule_compute](https://people.math.sc.edu/Burkardt/f_src/patterson_rule_compute/patterson_rule_compute.html) computes the points and weights of a 1d Gauss-Patterson quadrature rule of order 1, 3, 7, 15, 31, 63, 127, 255 or 511.

[quadmom](https://people.sc.fsu.edu/~jburkardt/f_src/quadmom/quadmom.html) computes a Gaussian quadrature rule for a weight function rho(x) based on the Golub-Welsch procedure that only requires knowledge of the moments of rho(x).

[quadpack](https://people.math.sc.edu/Burkardt/f_src/quadpack/quadpack.html) estimates integrals using numerical quadrature, by Piessens, deDoncker-Kapenga, Ueberhuber, and Kahaner.

[quadrule](https://people.sc.fsu.edu/~jburkardt/f_src/quadrule/quadrule.html) defines 1-dimensional quadrature rules.

[romberg](https://people.math.sc.edu/Burkardt/f_src/romberg/romberg.html) Integral Estimation using Repeated Quadrature and Extrapolation.

[stroud](https://people.math.sc.edu/Burkardt/f_src/stroud/stroud.html) defines quadrature rules for various geometric shapes.

[test_int](https://people.sc.fsu.edu/~jburkardt/f_src/test_int/test_int.html) contains a number of functions that may be used as test integrands for quadrature rules in 1D.

[test_int_2d](https://people.sc.fsu.edu/~jburkardt/f_src/test_int/test_int.html) defines test integrands for 2D quadrature rules.

[toms341](https://people.math.sc.edu/Burkardt/f77_src/toms351/toms351.html) implements ACM TOMS algorithm 351, for modified Romberg quadrature.

[toms370](https://people.math.sc.edu/Burkardt/f77_src/toms379/toms379.html) SQUANK (Simpson Quadrature Used Adaptively - Noise Killed).

[toms468](https://people.math.sc.edu/Burkardt/f77_src/toms468/toms468.html) Automatic Numerical Integration Over a Finite Interval.

[toms655](https://people.math.sc.edu/Burkardt/f_src/toms655/toms655.html) computes weights for interpolatory quadrature schemes, by Sylvan Elhay and Jaroslav Kautsky.

[truncated_normal_rule](https://people.sc.fsu.edu/~jburkardt/f_src/truncated_normal_rule/truncated_normal_rule.html) computes a quadrature rule for a normal distribution that has been truncated to [A,+oo), (-oo,B] or [a,b].

## Quadrature-N-Dimensional

[ball_integrals](https://people.math.sc.edu/Burkardt/f_src/ball_integrals/ball_integrals.html) returns the exact value of the integral of any monomial over the interior of the unit ball in 3d.

[circle_integrals](https://people.sc.fsu.edu/~jburkardt/f_src/circle_integrals/circle_integrals.html) returns the exact value of the integral of any monomial along the circumference of the unit circle in 2D.

[circle_monte_carlo](https://people.math.sc.edu/Burkardt/f_src/circle_monte_carlo/circle_monte_carlo.html) uses the Monte Carlo method to estimate the integral of a function over the circumference of the unit circle in 2d.

[circle_rule](https://people.sc.fsu.edu/~jburkardt/f_src/circle_rule/circle_rule.html) computes quadrature rules for approximating integrals over the circumference of the unit circle.

[cube_felippa_rule](https://people.sc.fsu.edu/~jburkardt/f_src/cube_felippa_rule/cube_felippa_rule.html) generates the points and weights of a Felippa quadrature rule over the interior of a cube in 3D.

[cube_integrals](https://people.sc.fsu.edu/~jburkardt/f_src/cube_integrals/cube_integrals.html) returns the exact value of the integral of any monomial over the interior of the unit cube in 3d.

[disk_rule](https://people.sc.fsu.edu/~jburkardt/f_src/disk_rule/disk_rule.html) computes quadrature rules for approximating integrals over the interior of the general disk in 2D.

[disk01_integrals](https://people.math.sc.edu/Burkardt/f_src/disk01_integrals/disk01_integrals.html) returns the exact value of the integral of any monomial over the interior of the unit disk in 2d.

[hyperball_integrals](https://people.sc.fsu.edu/~jburkardt/f_src/hyperball_integrals/hyperball_integrals.html) returns the exact value of the integral of any monomial over the interior of the unit hyperball in m dimensions.

[hypercube_integrals](https://people.math.sc.edu/Burkardt/f_src/hypercube_integrals/hypercube_integrals.html) returns the exact value of the integral of any monomial over the interior of the unit hypercube in m dimensions.

[hypersphere_integrals](https://people.sc.fsu.edu/~jburkardt/f_src/hypersphere_integrals/hypersphere_integrals.html) returns the exact value of the integral of any monomial over the surface of the unit hypersphere in m dimensions.

[nintlib](https://people.sc.fsu.edu/~jburkardt/f_src/nintlib/nintlib.html) estimates integrals over multi-dimensional regions.

[polygon_integrals](https://people.sc.fsu.edu/~jburkardt/f_src/polygon_integrals/polygon_integrals.html) returns the exact value of the integral of any monomial over the interior of a polygon in 2d.

[pyramid_felippa_rule](https://people.sc.fsu.edu/~jburkardt/f_src/pyramid_felippa_rule/pyramid_felippa_rule.html) returns Felippa quadrature rules over the interior of the unit pyramid in 3D.

[pyramid_integrals](https://people.sc.fsu.edu/~jburkardt/f_src/pyramid_integrals/pyramid_integrals.html) returns the exact value of the integral of any monomial over the interior of the unit pyramid in 3d.

[simplex_integrals](https://people.sc.fsu.edu/~jburkardt/f_src/simplex_integrals/simplex_integrals.html) returns the exact value of the integral of any monomial over the interior of the unit simplex in m dimensions.

[sphere_integrals](https://people.sc.fsu.edu/~jburkardt/f_src/sphere_integrals/sphere_integrals.html) returns the exact value of the integral of any monomial over the surface of the unit sphere in 3d.

[sphere_lebedev_rule](https://people.sc.fsu.edu/~jburkardt/f_src/sphere_lebedev_rule/sphere_lebedev_rule.html) computes Lebedev quadrature rules on the surface of the unit sphere in 3D.

[sphere_quad](https://people.sc.fsu.edu/~jburkardt/f_src/sphere_quad/sphere_quad.html) estimates the integral of a scalar function F(X,Y,Z) over the surface of the unit sphere centered at the origin.

[square_felippa_rule](https://people.sc.fsu.edu/~jburkardt/f_src/square_felippa_rule/square_felippa_rule.html) returns the points and weights of a Felippa quadrature rule over the interior of a square in 2D.

[square_integrals](https://people.math.sc.edu/Burkardt/f_src/square_integrals/square_integrals.html) returns the exact value of the integral of any monomial over the interior of the unit square in 2d.

[tetrahedron_felippa_rule](https://people.sc.fsu.edu/~jburkardt/f_src/tetrahedron_felippa_rule/tetrahedron_felippa_rule.html) returns Felippa's quadratures rules for approximating integrals over the interior of a tetrahedron in 3D.

[tetrahedron_integrals](https://people.math.sc.edu/Burkardt/f_src/tetrahedron_integrals/tetrahedron_integrals.html) returns the exact value of the integral of any monomial over the interior of the unit tetrahedron in 3d.

[triangle_felippa_rule](https://people.sc.fsu.edu/~jburkardt/f_src/triangle_felippa_rule/triangle_felippa_rule.html) returns Felippa's quadratures rules for approximating integrals over the interior of a triangle in 2D.

[triangle_integrals](https://people.sc.fsu.edu/~jburkardt/f_src/triangle_integrals/triangle_integrals.html) returns the exact value of the integral of any monomial over the interior of the unit triangle in 2d.

[wedge_felippa_rule](https://people.sc.fsu.edu/~jburkardt/f_src/wedge_felippa_rule/wedge_felippa_rule.html) returns quadratures rules for approximating integrals over the interior of the unit wedge in 3D.

[wedge_integrals](https://people.sc.fsu.edu/~jburkardt/f_src/wedge_integrals/wedge_integrals.html) returns the exact value of the integral of any monomial over the interior of the unit wedge in 3d.

## Quasirandom Number Generation

[faure](https://people.sc.fsu.edu/~jburkardt/f_src/faure/faure.html) Faure quasirandom sequence.

[halton](https://people.sc.fsu.edu/~jburkardt/f_src/halton/halton.html) Halton Quasi Monte Carlo (QMC) sequence using a simple interface.

[hammersley](https://people.sc.fsu.edu/~jburkardt/f_src/hammersley/hammersley.html) Hammersley Quasi Monte Carlo (QMC) sequence using a simple interface.

[niederreiter](https://people.math.sc.edu/Burkardt/f_src/niederreiter/niederreiter.html) Niederreiter quasirandom sequence, using an "arbitrary" base.

[niederreiter2](https://people.math.sc.edu/Burkardt/f_src/niederreiter2/niederreiter2.html) Niederreiter quasirandom sequence, using a base of 2.

[sobol](https://people.math.sc.edu/Burkardt/f_src/sobol/sobol.html) Sobol quasirandom sequence, by Bennett Fox.

[van_der_corput](https://people.math.sc.edu/Burkardt/f_src/van_der_corput/van_der_corput.html) van der Corput Quasi Monte Carlo (QMC) sequence, using a simple interface.

## Root-finding
[bisection](https://people.sc.fsu.edu/~jburkardt/f_src/bisection/bisection.html) applies the bisection method to seek a root of f(x) over a change-of-sign interval a <= x <= b.

[bisection_integer](https://people.sc.fsu.edu/~jburkardt/f_src/bisection_integer/bisection_integer.html) seeks an integer solution to the equation F(X)=0, using bisection within a user-supplied change of sign interval [A,B].

[bisection_rc](https://people.sc.fsu.edu/~jburkardt/f_src/bisection_rc/bisection_rc.html) bisection method for solving a scalar equation in a change of sign interval, using reverse communication (RC).

[brent](https://people.math.sc.edu/Burkardt/f_src/brent/brent.html) finds zeros or minima of a scalar function of a scalar variable, by Richard Brent.

[root_rc](https://people.sc.fsu.edu/~jburkardt/f_src/root_rc/root_rc.html): solves a scalar nonlinear equation f(x)=0, using reverse communication (RC), by Gaston Gonnet.

[test_zero](https://people.sc.fsu.edu/~jburkardt/f_src/test_zero/test_zero.html) defines nonlinear functions that may be used to test zero finders.

[zero_chandrupatla](https://people.sc.fsu.edu/~jburkardt/f_src/zero_chandrupatla/zero_chandrupatla.html): finds a zero of a scalar function of a scalar variable, starting from a change of sign interval, using the Chandrupatla method, which can converge faster than bisection, regula falsi, or Brent's method.

[zero_itp](https://people.sc.fsu.edu/~jburkardt/f_src/zero_itp/zero_itp.html): finds a zero of a scalar function of a scalar variable, starting from a change of sign interval, using the Interpolate/Truncate/Project (ITP) method, which has faster convergence than the bisection method.

[zero_laguerre](https://people.sc.fsu.edu/~jburkardt/f_src/zero_laguerre/zero_laguerre.html): uses Laguerre's method to find the zero of a function. The method needs first and second derivative information. The method almost always works when the function is a polynomial.

[zero_muller](https://people.sc.fsu.edu/~jburkardt/f_src/zero_muller/zero_muller.html) seeks a root of a nonlinear equation using Muller's method, with complex arithmetic.

[zero_rc](https://people.sc.fsu.edu/~jburkardt/f_src/zero_rc/zero_rc.html) seeks a solution of a scalar nonlinear equation f(x)=0, using reverse communication (RC), by Richard Brent.

[zoomin](https://people.sc.fsu.edu/~jburkardt/f_src/zoomin/zoomin.html) seeks a root of a scalar function.

## Sorting

[sort_rc](https://people.math.sc.edu/Burkardt/f_src/sort_rc/sort_rc.html) sorts a list of any kind of objects, using reverse communication (RC).

## Special Functions

[asa103](https://people.sc.fsu.edu/~jburkardt/f_src/asa103/asa103.html) evaluates the digamma or psi function, by Jose Bernardo.

[asa239](https://people.sc.fsu.edu/~jburkardt/f_src/asa239/asa239.html) evaluates the incomplete Gamma function, by Shea.

[besselj](https://people.sc.fsu.edu/~jburkardt/f_src/besselj/besselj.html) evaluates Bessel J functions of noninteger order.

[cordic](https://people.sc.fsu.edu/~jburkardt/f_src/cordic/cordic.html) uses the CORDIC algorithm to evaluate certain functions, in particular the sine and cosine.

[fn](https://people.sc.fsu.edu/~jburkardt/f_src/fn/fn.html) evaluates elementary and special functions using Chebyshev polynomials.

[specfun](https://people.sc.fsu.edu/~jburkardt/f_src/specfun/specfun.html) evaluates special functions, including Bessel I, J, K and Y functions, Dawson Integral, Error (Erf), Exponential Integral (E1 and EI), Gamma, log Gamma, and Psi/Digamma, by William Cody and Laura Stoltz.

[special_functions](https://people.sc.fsu.edu/~jburkardt/f_src/special_functions/special_functions.html) evaluates special functions, including Airy, Associated Legendre Bessel, Beta, Complete Elliptic Integral, Confluent Hypergeometric, Cosine Integral, Elliptic Integral, Error, Exponential Integral, Fresnel Integral, Gamma, Hankel, Hypergeometric, Incomplete Beta, Incomplete Gamma, Jacobian Elliptic, Kelvin, Lambda, Legendre, Mathieu, Modified Spherical Bessel, Parabolic Cylinder, Psi, Riccati-Bessel, Sine Integral, Spheroidal Wave, Struve, Whittaker, as well as Bernoulli Numbers, Euler Numbers, Hermite Polynomials, Laguerre Polynomials, Legendre Polynomials, by Shanjie Zhang, Jianming Jin.

[toms435](https://people.sc.fsu.edu/~jburkardt/f77_src/toms435/toms435.html) evaluates the modified incomplete Gamma function.

[toms708](https://people.sc.fsu.edu/~jburkardt/f_src/asa239/asa239.html) computes the Incomplete Beta Function ratio.

[toms715](https://people.math.sc.edu/Burkardt/f_src/toms715/toms715.html) evaluates special functions, including the Bessel I, J, K, and Y functions of order 0, of order 1, and of any real order, Dawson's integral, the error function, exponential integrals, the gamma function, the normal distribution function, the psi function.

## Statistics

[starpac](https://people.math.sc.edu/Burkardt/f_src/starpac/starpac.html) nonlinear least squares regression, time series analysis (in both time and frequency domains), line printer graphics, basic statistical analysis, and linear least squares regression.

## Uniform Random Number Generation

[asa183](https://people.sc.fsu.edu/~jburkardt/f_src/asa183/asa183.html) random number generator (RNG), by Wichman and Hill.

[randlc](https://people.sc.fsu.edu/~jburkardt/f_src/randlc/randlc.html) random number generator (RNG) used by the NAS Parallel Benchmarks.

[random_sorted](https://people.sc.fsu.edu/~jburkardt/f_src/random_sorted/random_sorted.html) create a vector of random values which are already sorted.

[rnglib](https://people.sc.fsu.edu/~jburkardt/f_src/rnglib/rnglib.html) random number generators (RNG's) which can generate one or more streams of random numbers.

[uniform](https://people.sc.fsu.edu/~jburkardt/f_src/uniform/uniform.html) random number generator is based on a simple, old, and limited linear congruential random number generator originally used in the IBM System 360.

## Wavelet Transforms

[haar](https://people.sc.fsu.edu/~jburkardt/f_src/haar/haar.html) computes the [Haar transform](https://en.wikipedia.org/wiki/Haar_wavelet) of data.
