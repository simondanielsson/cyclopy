open Core

let%test_module "Module_imports" =
  (module struct
    let%test_unit "compute_full_module_name with src project_root" =
      let project_root = "my_project" in
      let file_path = "my_project/src/my/module/path.py" in
      let module_name =
        Cyclopy.Module_imports.module_name_of_path ~project_root file_path
      in
      [%test_result: string] module_name ~expect:"src.my.module.path"
    ;;

    let%test_unit "compute_full_module_name with . project root" =
      let project_root = "." in
      let file_path = "utils/helper.py" in
      let module_name =
        Cyclopy.Module_imports.module_name_of_path ~project_root file_path
      in
      [%test_result: string] module_name ~expect:"utils.helper"
    ;;
  end)
;;
