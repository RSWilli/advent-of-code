// the following constans represent the 2D characters some aoc solutions use
// thanks to https://github com/bsoyka/advent-of-code-ocr/blob/main/advent_of_code_ocr/characters py

pub(crate) const A: &str = " ## \n#  #\n#  #\n####\n#  #\n#  #\n";
pub(crate) const B: &str = "### \n#  #\n### \n#  #\n#  #\n### \n";
pub(crate) const C: &str = " ## \n#  #\n#   \n#   \n#  #\n ## \n";
pub(crate) const E: &str = "####\n#   \n### \n#   \n#   \n####\n";
pub(crate) const F: &str = "####\n#   \n### \n#   \n#   \n#   \n";
pub(crate) const G: &str = " ## \n#  #\n#   \n# ##\n#  #\n ###\n";
pub(crate) const H: &str = "#  #\n#  #\n####\n#  #\n#  #\n#  #\n";
pub(crate) const I: &str = " ###\n  # \n  # \n  # \n  # \n ###\n";
pub(crate) const J: &str = "  ##\n   #\n   #\n   #\n#  #\n ## \n";
pub(crate) const K: &str = "#  #\n# # \n##  \n# # \n# # \n#  #\n";
pub(crate) const L: &str = "#   \n#   \n#   \n#   \n#   \n####\n";
pub(crate) const O: &str = " ## \n#  #\n#  #\n#  #\n#  #\n ## \n";
pub(crate) const P: &str = "### \n#  #\n#  #\n### \n#   \n#   \n";
pub(crate) const R: &str = "### \n#  #\n#  #\n### \n# # \n#  #\n";
pub(crate) const S: &str = " ###\n#   \n#   \n ## \n   #\n### \n";
pub(crate) const U: &str = "#  #\n#  #\n#  #\n#  #\n#  #\n ## \n";
pub(crate) const Y: &str = "#   \n#   \n # #\n  # \n  # \n  # \n";
pub(crate) const Z: &str = "####\n   #\n  # \n #  \n#   \n####\n";

// the dark pixels are using utf8 block characters
// the light pixels are using spaces
pub const DARK_PIXEL: char = '#';
pub const LIGHT_PIXEL: char = ' ';
