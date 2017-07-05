#check, upload files
library(sbtools)
library(R.utils)
authenticate_sb('lawinslow@gmail.com')

zip_hash_upload = function(name, files, groups){

  head_itm = item_create(title = name)

  ugroups = unique(groups)
  out = data.frame(group = ugroups)
  out$url = NA
  out$md5 = NA

  for(i in seq_along(ugroups)){
    groupfiles = files[groups == ugroups[i]]
    tmpfile = file.path(tempdir(), ugroups[i])
    zip(tmpfile, groupfiles, flags = '-j')

    itm = item_create(parent_id = head_itm, title = ugroups[i])
    item_append_files(itm, tmpfile)
    flist = item_list_files(itm)
    out$url[i] = flist$url
    out$md5[i] = tools::md5sum(tmpfile)
  }
  return(out)
}

name = 'NHDH'
nhd_files = Sys.glob('z:/big_datasets/NHD/HU4/Shape_unzip/NHD_H_*_Shape.zip/Shape/*')
groups    = basename(dirname(dirname(nhd_files)))
nhdh_files = zip_hash_upload(name, nhd_files, groups)

