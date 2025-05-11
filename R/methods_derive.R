method(
  pkg_data_derive,
  list(pkg_data_class("desc"), pkg, pkg_local_resource)
) <-
  function(field, pkg, resource, ...) {
    desc::desc(resource@path)
  }

method(pkg_data_derive, list(pkg_data_class("name"), pkg, class_any)) <-
  function(field, pkg, resource, ...) {
    pkg$desc$get_field("Package")
  }

method(pkg_data_derive, list(pkg_data_class("version"), pkg, class_any)) <-
  function(field, pkg, resource, ...) {
    pkg$desc$get_field("Version")
  }

method(
  pkg_data_derive,
  list(pkg_data_class("r_cmd_check"), pkg, pkg_local_resource)
) <-
  function(field, pkg, resource, ...) {

    pkg$desc$get_field("Version")
  }
