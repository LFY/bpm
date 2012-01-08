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
                    if ($file != '.' && $file != '..') {
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
          
        function addImage($filePath, $imgWidth, $valign)
        {
            echo "<td valign=" . $valign . " align=center>";
            echo "<img src=" . $filePath . "></td>";
        }
        
        function addLikertRow($scale,$name,$text)
        {
            echo '<td valign=middle>';
            echo "<form name=\"myform\">";
            echo '<table>';
            
            echo '<tr>';
            for($c=1;$c<=$scale;$c++) {
                echo "<td align=center style=width:100px><input type=\"radio\" name=\"" . $name . "\" value=" . $c . " onclick=\"experiment.next()\">" . $c . "</td>"; 
            }
            echo '</tr>';
            
            echo '<tr>';
            for($c=1;$c<=$scale;$c++) {
                if($c==1) {
                    echo "<td align=center style=width:100px><b>not</b> at all likely</td>";
                }
                else if($c==$scale) {
                    echo "<td align=center style=width:100px>very likely</td>";
                }
                else { echo "<td align=center style=width:100px></td>"; }
            }
            echo "</tr></table></form></td>";
        }
          
        function displayTask($exemplars, $task, $numRows, $numCols, $numSamples, $imgWidth, $currNumTask, $numT)
        {
            // display examples
            echo "<table style=\"border:5px solid gray;\">";
            for($r=0;$r<$numRows;$r++) {
                echo '<tr>';
                for($c=0;$c<$numCols;$c++) {
                    $index = $r*$numCols+$c;
                    if ($index < count($exemplars)){
                        addImage($exemplars[$index], $imgWidth, "bottom");
                    }
                }
                echo '</tr>';
            }
            echo '</table>';
            
            echo "<p>How <b>likely</b> is it that the following object is the same kind as the ones in the box above?</p>";
            
            // display task
            echo '<table>';
                echo '<tr>';
                addImage($task,$imgWidth,"middle");
                addLikertRow(7,$currNumTask,"");
                echo '</tr>';
            echo '</table>';
            
         }
        
        $samplesPerCondition = 3;
        $examples = fileArray("images/");
        shuffle($examples); // randomize order of examples
        $numExamples = count($examples);
        
        for ($i=0; $i<$numExamples; $i++)
        {
            $exemplars = fileArray($examples[$i]."/mturk-lgcg/");
            shuffle($exemplars); // randomize order of exemplars
            
            $conditions = fileArray($examples[$i]."/");
            $tasks = array();
            for ($j=0; $j<count($conditions); $j++)
            {
                $tasks = array_merge($tasks, randomSelection(fileArray($conditions[$j]."/"),$samplesPerCondition));   // randomly sample from each condition
            }
            shuffle($tasks); // randomize order of tasks
            
            $numTasks = count($tasks);
           
            for ($j=0; $j<$numTasks; $j++)
            {
                $globalTaskID = $i*$numTasks + $j + 1;
                echo "<div class=\"slide\" id=\"stage" . $globalTaskID . "\">";
                echo "Task <span id=\"currTaskNum\">" . $globalTaskID . "</span> of <span id=\"numTasks\">" . ($numExamples*$numTasks) . "</span>.";

                displayTask($exemplars, $tasks[$j], 2, 5, 3, 300);
                
                // TODO: fix the summary
                // TODO: lgcg, mgcg & bayes renders for playgrounds
                // TODO: mgcg & bayes renders for seuss
                // TODO: fix intro page
                /* echo "<span id=\"category" . $currNumTask . "\" class=\"scriptVar\">". end(explode("/",$examples[$i])) ."</span>";
                echo "<span id=\"trainingExamples" . $currNumTask ."\" class=\"scriptVar\">". implode("\n",$exemplars) ."</span>"; */
                echo "<span id=\"experimentExamples" . $globalTaskID ."\" class=\"scriptVar\">". end($tasks[$j]) ."</span>";
                
                echo '</div>'; 
            }
         
        }
         
      ?>

    <div class="slide" id="finished">
      You're finished - thanks for participating! Submitting to Mechanical Turk...
    </div>

    <script src="experiment.js"></script>
  </body>
</html>
