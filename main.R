# EPOC-AKI Study
# Copyright (C) 2018-2020  Alwin Wang, Lisa Toh

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

file_sources <- list.files(path = "R/", pattern = "^[0-9][0-9].*.R$", full.names = TRUE)

max_num = 31
excl_num = c(7, 10, 11, 12)
file_nums = as.numeric(gsub(".*R/(.+[0-9])_[A-Za-z].*", "\\1", file_sources))
file_sources = file_sources[file_nums <= max_num & !(file_nums %in% excl_num)]
rm(max_num, excl_num, file_nums)

sapply(
  file_sources,
  function(file) {
    cat(paste0("\n", file, "\n"))
    source(file, .GlobalEnv)
  }
)

rm(file_sources)
