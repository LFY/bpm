<html>
  <head>
    <title>3D-Model Experiment</title>
    <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.6.4/jquery.min.js"></script>
    <script src="http://longouyang.github.com/mmturkey/mmturkey.js"></script>
    <link rel="stylesheet" href="style.css" />
  </head>

  <body>
    <div class="slide" id="instructions">
      <p id='logo-text'>Experiment Title</p>
      <p class="block-text">In this experiment, you will see a set of 3D models displayed at the top of the screen. Below, you will see a list of individual 3D models. For each model in the list below, rate how well it belongs to the set of models above and how different it is from the set. </p>
      <button type="button" onclick="this.blur(); experiment.next()">Start</button>
    </div>

      <?php
          
        function fileArray($dir)
        {
            $files = Array();
            if ($dh = opendir($dir)) {
                while (($file = readdir($dh)) !== false) {
                    if ($file != '.' && $file != '..' && $file != '.DS_Store') {
                        array_push($files, $dir . $file);
                    }
                }
            }
            return $files;
        }
        
        function randomSelection($fullArray, $num)
        {
            $subArray = Array();
            for($n=0; $n<$num; $n++) {
                $randIndex=rand(0,count($fullArray)-1);
                array_push($subArray, $fullArray[$randIndex]);
                array_splice($fullArray, $randIndex, 1);
            }
            return $subArray;
        }
          
        function addImage($filePath, $imgWidth)
        {
            echo "<td style=width:" . $imgWidth . "px>";
            echo "<img src=" . $filePath . " width=" . $imgWidth . "px></td>";
        }
        
        function addLikertRow($scale,$name,$text)
        {
            echo '<td>';
            echo "<form name=\"myform\">";
            echo '<table cellspacing=10px>';
            echo '<tr>' . $text . '</tr>';
            echo '<tr valign=bottom>';
            
            for($c=1;$c<=$scale;$c++) {
                echo "<td><input type=\"radio\" name=\"" . $name . "\" value=" . $c . ">" . $c . "</td>"; 
            }
            echo "</tr></table></form></td>";
        }
          
        function displayTask($numRows, $numCols, $numSamples, $imgWidth, $category, $currNumTask, $numT)
        {
            echo "Task <span id=\"currTaskNum\">" . $currNumTask . "</span> of <span id=\"numTasks\">" . $numT . "</span>.";

            $images = fileArray($category."/exemplars/");
           
            $choices = array_merge(randomSelection(fileArray($category."/bayes/"),$numSamples),
                                   randomSelection(fileArray($category."/mgcg/"),$numSamples),
                                   randomSelection(fileArray($category."/holdout/"),$numSamples));
            shuffle($images); shuffle($choices);
            
            $trainingExamples = Array();
            echo '<table cellspacing=5px>';
            for($r=0;$r<$numRows;$r++) {
                echo '<tr valign=bottom>';
                for($c=0;$c<$numCols;$c++) {
                    $index = $r*$numCols+$c;
                    if ($index < count($images)){
                        array_push($trainingExamples, end(explode("/",$images[$index])));
                        addImage($images[$index], $imgWidth);
                    }
                }
                echo '</tr>';
            }
            echo '</table>';
            
            echo '<p> To what extent is each 3D-model</p>';
            
            $experimentExamples = Array();
            echo '<table cellspacing=5px>';
            for ($r=0;$r<count($choices);$r++) {
                array_push($experimentExamples, end(explode("/",$choices[$r])));
                echo '<tr valign=bottom>';
                addImage($choices[$r],$imgWidth);
                addLikertRow(7,$currNumTask . "-" . ($r+1) . "-in", "part of the set?");
                addLikertRow(7,$currNumTask . "-" . ($r+1) . "-distinct", "distinct?");
                echo '</tr>';
            }
            echo '</table>';
            
            echo "<span id=\"category" . $currNumTask . "\" class=\"scriptVar\">". end(explode("/",$category)) ."</span>";
            echo "<span id=\"trainingExamples" . $currNumTask ."\" class=\"scriptVar\">". implode("\n",$trainingExamples) ."</span>";
            echo "<span id=\"experimentExamples" . $currNumTask ."\" class=\"scriptVar\">". implode("\n",$experimentExamples) ."</span>";
            
        }
        
        $sets = fileArray("images/");
        shuffle($sets);
        for ($i=1; $i<=count($sets); $i++)
        {
            echo "<div class=\"slide\" id=\"stage" . $i . "\">";
            displayTask(2, 5, 3, 300, $sets[$i-1], $i, count($sets));
            echo "<button type=\"button\" onclick=\"this.blur(); experiment.next()\">Next</button>";
            echo '</div>';
        }
    
      ?>

    <div class="slide" id="finished">
      You're finished - thanks for participating! Submitting to Mechanical Turk...
    </div>

    <script src="experiment.js"></script>
  </body>
</html>
