<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<p> The <strong>project summary page</strong> you find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>


<p> Feel free to download and try the current release of <strong>lossDev</strong>, <a href="pkg/lossDev_0.9.0.zip"> version 0.9.0</a>, which requires <strong>R</strong> version 2.9.2 or greater.
Currently only Windows is supported, but Linux\Unix will be supported shortly.
The package can be installed after opening <strong>R</strong> and selecting "Packages->Install package(s) from local zip files...".
<strong>lossDev</strong> should be available on <strong>CRAN</strong> shortly.</p>

<p>The documentation is currently available <a href="pkg/lossDev_Help.pdf"><strong>here</strong></a>.</p>

<p> A vignette, which offers specific examples of how to use this package, is available for download <a href="pkg/lossDev_Vignette.pdf"> here</a>.
The <strong>R</strong> code associated with this Vignette can be downloaded <a href="pkg/lossDev_Code.R"> here</a>.
</p>

<p> The paper the package is based on you find <a href="http://ssrn.com/author=101739"> here</a>. </p>


</body>
</html>
