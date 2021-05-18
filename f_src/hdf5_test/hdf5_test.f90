program main

!*****************************************************************************80
!
!! MAIN is the main program for HDF5_TEST.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 February 2017
!
!  Author:
!
!    John Burkardt
!
  implicit none

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HDF5_TEST:'
  write ( *, '(a)' ) '  FORTRAN90 version:'
  write ( *, '(a)' ) '  Test the HDF5 library.'

  call hdf5_test01 ( )
  call hdf5_test02 ( )
  call hdf5_test03 ( )
  call hdf5_test04 ( )
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HDF5_TEST:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop
end
subroutine hdf5_test01 ( )

!*****************************************************************************80
!
!! HDF5_TEST01 creates an HDF file.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 February 2017
!
!  Author:
!
!    John Burkardt
!
  use hdf5

  implicit none

  character ( len = 14 ), parameter :: file_name = 'test01.h5'
  integer ( hid_t ) file_id
  integer error

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HDF5_TEST01:'
  write ( *, '(a)' ) '  Create an HDF5 file "test01.h5"'
!
!  Get the file id.
!
  call h5open_f ( error )
  call h5fcreate_f ( file_name, H5F_ACC_TRUNC_F, file_id, error )
!
!  Close the file.
!
  call h5fclose_f ( file_id, error )
  call h5close_f ( error )

  return
end
subroutine hdf5_test02 ( )

!*****************************************************************************80
!
!! HDF5_TEST02 creates an HDF dataset.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    20 February 2017
!
!  Author:
!
!    John Burkardt
!
  use hdf5

  implicit none

  integer, parameter:: dset_rank = 2

  integer ( hsize_t ) dims(dset_rank)
  integer ( hid_t ) dset_id
  integer ( hid_t ) dspace_id
  character ( len = 19 ), parameter :: dset_name = '/dset'
  character ( len = 14 ), parameter :: file_name = 'test02.h5'
  integer ( hid_t ) file_id
  integer error

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HDF5_TEST02:'
  write ( *, '(a)' ) '  Create an HDF5 file "test02.h5"'
  write ( *, '(a)' ) '  Create a dataset.'
!
!  Get the file id.
!
  call h5open_f ( error )
  call h5fcreate_f ( file_name, H5F_ACC_TRUNC_F, file_id, error )
!
!  Get the dataspace identifier.
!
  dims(1) = 4
  dims(2) = 6
  call h5screate_simple_f ( dset_rank, dims, dspace_id, error )
!
!  Get the dataset identifier.
!
  call h5dcreate_f ( file_id, dset_name, H5T_NATIVE_INTEGER, dspace_id, &
    dset_id, error )

  call h5dclose_f ( dset_id, error )
  call h5sclose_f ( dspace_id, error )
!
!  Close the file.
!
  call h5fclose_f ( file_id, error )
  call h5close_f ( error )

  return
end
subroutine hdf5_test03 ( )

!*****************************************************************************80
!
!! HDF5_TEST03 puts data in the dataset.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 February 2017
!
!  Author:
!
!    John Burkardt
!
  use hdf5

  implicit none

  integer, parameter:: dset_rank = 2

  integer dset_data(4,6)
  integer ( hsize_t ) :: dset_dims(dset_rank) = (/ 4, 6 /)
  integer ( hid_t ) dset_id
  character ( len = 19 ), parameter :: dset_name = '/dset'
  integer ( hid_t ) dspace_id
  integer error
  integer ( hid_t ) file_id
  character ( len = 14 ), parameter :: file_name = 'test03.h5'
  integer i
  integer j

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HDF5_TEST03:'
  write ( *, '(a)' ) '  Create an HDF5 file "test03.h5"'
  write ( *, '(a)' ) '  Create a dataset.'
  write ( *, '(a)' ) '  Put data in the dataset.'
!
!  Get the file id.
!
  call h5open_f ( error )
  call h5fcreate_f ( file_name, H5F_ACC_TRUNC_F, file_id, error )
!
!  Get the dataspace id.
!
  call h5screate_simple_f ( dset_rank, dset_dims, dspace_id, error )
!
!  Get the dataset id.
!
  call h5dcreate_f ( file_id, dset_name, H5T_NATIVE_INTEGER, dspace_id, &
    dset_id, error )
!
!  Create the data.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Dataset /dset written to test03.h5'
  write ( *, '(a)' ) ''

  do i = 1, 4
    do j = 1, 6
      dset_data(i,j) = 4 * ( j - 1 ) + i
      write ( *, '(a,i2,a,i2,a,i4)' ) 'dset(', i, ',', j, ') = ', dset_data(i,j)
    end do
  end do
!
!  Write the data to the dataset.
!
  call h5dwrite_f ( dset_id, H5T_NATIVE_INTEGER, dset_data, dset_dims, error )
!
!  Close the data set.
!
  call h5dclose_f ( dset_id, error )
!
!  Close the data space.
!
  call h5sclose_f ( dspace_id, error )
!
!  Close the file.
!
  call h5fclose_f ( file_id, error )
  call h5close_f ( error )

  return
end
subroutine hdf5_test04 ( )

!*****************************************************************************80
!
!! HDF5_TEST04 reads the data from the dataset created by HDF_TEST03.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    24 February 2017
!
!  Author:
!
!    John Burkardt
!
  use hdf5

  implicit none

  integer, allocatable :: dset_data(:,:)
  integer ( hid_t ) fred(4,6)
  integer ( hsize_t ), allocatable :: dset_dims(:)
  integer ( hid_t ) dset_id
  character ( len = 19 ), parameter :: dset_name = '/dset'
  integer dset_rank
  integer ( hid_t ) dspace_id
  integer error
  integer ( hid_t ) file_id
  character ( len = 14 ), parameter :: file_name = 'test03.h5'
  integer i
  integer j

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'HDF5_TEST04:'
  write ( *, '(a)' ) '  Open the HDF5 file "test03.h5"'
  write ( *, '(a)' ) '  Access a dataset.'
  write ( *, '(a)' ) '  Read data from the dataset.'
!
!  Get the file id.
!
  call h5fopen_f ( file_name, H5F_ACC_RDONLY_F, file_id, error )
!
!  Get the data set id.
!
  call h5dopen_f ( file_id, dset_name, dset_id, error )
!
!  Get the data space id.
!
  call h5dget_space_f ( dset_id, dspace_id, error )
!
!  Get the rank of the data set.
!
  call h5sget_simple_extent_ndims_f ( dspace_id, dset_rank, error )
!
!  Get the dimensions.
!
  allocate ( dset_dims(dset_rank) )
  call h5sget_simple_extent_dims_f ( dspace_id, dset_dims, dset_dims, error )
!
!  Get the data.
!
  allocate ( dset_data(dset_dims(1),dset_dims(2)) )
  call h5dread_f ( dset_id, H5T_NATIVE_INTEGER, dset_data, dset_dims, error )
!
!  Print the data that has been read.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  Dataset /dset written to test03.h5'
  write ( *, '(a)' ) ''

  do i = 1, 4
    do j = 1, 6
     write ( *, '(a,i2,a,i2,a,i4)' ) 'dset(', i, ',', j, ') = ', dset_data(i,j)
    end do
  end do
!
!  Close the data set.
!
  call h5dclose_f ( dset_id, error )
!
!  Close the data space.
!
  call h5sclose_f ( dspace_id, error )
!
!  Close the file.
!
  call h5fclose_f ( file_id, error )

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

