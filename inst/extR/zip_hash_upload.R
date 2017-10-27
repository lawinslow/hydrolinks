#check, upload files
library(sbtools)
library(R.utils)
authenticate_sb('lawinslow@gmail.com')


hash_upload = function(name, files){

  head_itm = item_create(title = name)

  out = data.frame(filename = files)
  out$url = NA
  out$md5 = NA

  for(i in seq_along(files)){
    itm = item_create(parent_id = head_itm, title = basename(files[i]))
    item_append_files(itm, files[i])
    flist = item_list_files(itm)
    out$url[i] = flist$url
    out$md5[i] = tools::md5sum(files[i])
  }
  return(out)
}


zip_hash_upload = function(name, files, groups){

  head_itm = item_create(title = name)

  ugroups = unique(groups)
  out = data.frame(group = ugroups)
  out$url = NA
  out$md5 = NA

  for(i in seq_along(ugroups)){
    groupfiles = files[groups == ugroups[i]]
    tmpfile = file.path(tempdir(), paste0(ugroups[i], '.zip'))
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
nhd_files = Sys.glob('e:/zip/nhdh/*')
nhdh_files = hash_upload(name, nhd_files)

write.csv(nhdh_files, 'inst/extdata/nhdh.csv', row.names=FALSE)

name = 'hydrolakes'
hl_files = Sys.glob('e:/zip/hydrolakes/*')
zhres = hash_upload(name, hl_files)

write.csv(zhres, 'inst/extdata/hydrolakes.csv', row.names=FALSE)

name = 'nhdplusv2'
nhdpfiles = Sys.glob('e:/zip/nhdplusv2/*')
nhdp = hash_upload(name, nhdpfiles)

write.csv(nhdp, 'inst/extdata/nhdplusv2.csv', row.names=FALSE)


name = 'shape_id_cache'
files  = Sys.glob('E:/zip/shape_id_cache/*.sqlite3')
groups = basename(files)
nhdp = zip_hash_upload(name, files, groups)

write.csv(nhdp, 'inst/extdata/shape_id_cache.csv', row.names=FALSE)
