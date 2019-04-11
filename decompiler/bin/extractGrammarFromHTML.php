<?php
$url = $argv[1];
include('simple_html_dom.php');
$file = fopen("java.grammar", "w") or die("Unable to open file!");

$html = file_get_html($url);
foreach ($html->find('div.productionset') as $productionSet) {
    foreach ($productionSet->find('div.productionrecap') as $productionrecap) {
        $production = $productionrecap->find('.production');
        if (sizeof($production)) {
            $production = $production[0];
            $lhs = $production->find('div.lhs', 0);
            $rhs = $production->find('div.rhs', 0);
            if ($lhs) {
                fwrite($file, $lhs->innertext . "\n");
            }

            if ($rhs) {
                foreach ($rhs->children() as $child) {
                    fwrite($file, " " . $child->innertext);
                }
                fwrite($file, "\n");
            }
        }
        fwrite($file, "\n");
    }
}
fclose($file);
?>