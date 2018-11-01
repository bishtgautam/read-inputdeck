program read_inputdeck
  !
#include <petsc/finclude/petsc.h>
  !
  use PFLOTRAN_Constants_module, only : IN_UNIT, MAXSTRINGLENGTH, MAXWORDLENGTH
  use Option_module
  use Input_Aux_module
  use petscsys
  !
  implicit none
  !
  PetscErrorCode     :: ierr
  type(option_type), pointer :: option
  type(input_type), pointer :: input
  character(len=MAXSTRINGLENGTH) :: string
  character(len=MAXWORDLENGTH) :: word
  
  option => OptionCreate()
  call OptionInitMPI(option)
  option%input_filename = 'pflotran.in'
  call OptionInitPetsc(option)

  write(*,*)'option%input_filename = ',trim(option%input_filename)

  input => InputCreate(IN_UNIT,option%input_filename,option)

  string = 'SIMULATION'
  call InputFindStringInFile(input,option,string)
  call InputFindStringErrorMsg(input,option,string)
  word = ''
  do
    call InputReadPflotranString(input,option)
    if (InputCheckExit(input,option)) exit
    call InputReadWord(input,option,word,PETSC_TRUE)
    write(*,*)'word = ',trim(word)
 end do

  call PetscFinalize(ierr)

end program read_inputdeck

