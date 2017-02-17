module version_control

  implicit none

#ifndef VERSION_H
#define VERSION_H

#define QUOTE(str) "str"
#define EXPAND_AND_QUOTE(str) QUOTE(str)

#define GIT_BRANCH_VALUE EXPAND_AND_QUOTE( @GIT_BRANCH@ )
#define GIT_COMMIT_HASH_VALUE EXPAND_AND_QUOTE( @GIT_COMMIT_HASH@ )

  character (len=20), parameter :: GIT_COMMIT_HASH_STRING = GIT_COMMIT_HASH_VALUE
  character (len=30), parameter :: GIT_BRANCH_STRING = GIT_BRANCH_VALUE

  character(len=:), parameter  :: PRMS_VERSION = "4.0.2"
  character (len=:), parameter :: COMPILE_DATE = trim(__DATE__)
  character (len=:), parameter :: COMPILE_TIME = trim(__TIME__)
  character (len=:), parameter :: COMPILATION_TIMESTAMP =            &
    trim(COMPILE_DATE)//"  "//trim(COMPILE_TIME)

#endif

end module version_control
