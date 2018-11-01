module PFLOTRAN_Constants_module

! IMPORTANT NOTE: This module can have no dependencies on other modules!!!

  use, intrinsic :: iso_fortran_env, only : stdout=>Output_Unit
 
  implicit none

  private

#include "petsc/finclude/petscsys.h"

  PetscInt, parameter, public :: MAXSTRINGLENGTH = 512
  PetscInt, parameter, public :: MAXWORDLENGTH = 32
  PetscInt, parameter, public :: STDOUT_UNIT = stdout
  PetscInt, parameter, public :: OUT_UNIT = 15
  PetscInt, parameter, public :: IN_UNIT = 17

  ! If you increase MAX_IN_UNIT, you MUST ensure that no other units #
  ! lie between IN_UNIT and MAX_IN_UNIT, as these units are reserved
  ! for embedded input files.
  PetscInt, parameter, public :: MAX_IN_UNIT = 25

  ! constants
  PetscReal, parameter, public :: DAYS_PER_YEAR = 365.d0

  PetscInt, parameter, public :: ONE_INTEGER = 1

  PetscMPIInt, parameter, public :: ONE_INTEGER_MPI = ONE_INTEGER

  ! uninitialized values
  PetscInt, parameter, public :: UNINITIALIZED_INTEGER = -999
  PetscReal, parameter, public :: UNINITIALIZED_DOUBLE = -999.d0

end module PFLOTRAN_Constants_module
