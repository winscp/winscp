<?

$filename = $_SERVER["argv"][1];

$timestamp = filemtime($filename);
$contents = file_get_contents($filename);

$contents = str_replace("align=64", "align=256", $contents);

file_put_contents($filename, $contents);

touch($filename, $timestamp);

?>