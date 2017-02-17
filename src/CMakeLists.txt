set(SRCS

  basin.f90
  basin_sum.f90
  call_modules.f90
  cascade.f90
  ccsolrad.f90
  climate_hru.f90
  climateflow.f90
  ddsolrad.f90
  frost_date.f90
  gwflow.f90
  ide_dist.f
  intcp.f90
  lake_route.f90
  map_results.f90
  muskingum.f90
  nhru_summary.f90
  obs.f90
  potet_hamon.f90
  potet_hs.f90
  potet_jh.f90
  potet_pan.f90
  potet_pm.f90
  potet_pt.f90
  precip_1sta_laps.f90
  precip_dist2.f90
  prms_summary.f90
  prms_time.f90
  routing.f90
  snowcomp.f90
  soilzone.f90
  soltab.f90
  srunoff.f90
  strmflow.f90
  strmflow_in_out.f90
  strmflow_lake.f90
  subbasin.f90
  temp_1sta_laps.f90
  temp_dist2.f90
  transp_frost.f90
  transp_tindex.f90
  utils_prms.f90
  water_balance.f90
  write_climate_hru.f90
  xyz_dist.f

  generated/version_control.F90

  mmf/alloc_space.c
  mmf/batch_run.c
  mmf/batch_run_functions.c
  mmf/build_lists.c
  mmf/call_modules.c
  mmf/call_setdims.c
  mmf/check_vars.c
  mmf/control_addr.c
  mmf/control_array.c
  mmf/control_var.c
  mmf/create_vstats.c
  mmf/decl_control.c
  mmf/decldim.c
  mmf/declparam.c
  mmf/declvar.c
  mmf/dim_addr.c
  mmf/free_vstats.c
  mmf/get_elem_add.c
  mmf/get_times.c
  mmf/getdim.c
  mmf/getdimname.c
  mmf/getparam.c
  mmf/getvar.c
  mmf/graph_single_run.c
  mmf/julconvert.c
  mmf/julday.c
  mmf/load_param.c
  mmf/mmf.c
  mmf/param_addr.c
  mmf/parse_args.c
  mmf/print_model_info.c
  mmf/print_params.c
  mmf/print_vars.c
  mmf/putvar.c
  mmf/read_control.c
  mmf/read_datainfo.c
  mmf/read_line.c
  mmf/read_params.c
  mmf/read_vars.c
  mmf/readvar.c
  mmf/reset_dim.c
  mmf/save_params.c
  mmf/setup_cont.c
  mmf/sort_dims.c
  mmf/sort_params.c
  mmf/sort_vars.c
  mmf/str_to_vals.c
  mmf/timing.c
  mmf/umalloc_etc.c
  mmf/var_addr.c
  mmf/write_vstats.c

  )

# the following snippet assigns the local filename to __SRCNAME__
# for each of the files contained in the 'SRCS' list
#
# i.e. rather than using __FILE__, which might expand to
# d:/Some_dir/some_subdir/filename.F90, we can use
# __SRCNAME__, which expands to 'filename.F90'
#
foreach(f IN LISTS SRCS)
  get_filename_component(b ${f} NAME)
  set_source_files_properties(${f} PROPERTIES
    COMPILE_DEFINITIONS "__SRCNAME__=\"${b}\"")
endforeach()

add_executable(prms ${SRCS})

set_target_properties(prms
      PROPERTIES COMPILE_DEFINITIONS "${PREPROCESSOR_DEFINES}")
