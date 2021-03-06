<html>
  <head>
    <title>3D-Model Experiment</title>
    <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.6.4/jquery.min.js"></script>
    <script src="http://longouyang.github.com/mmturkey/mmturkey.js"></script>
    <link rel="stylesheet" href="style.css" />
  </head>

  <body>
    <div class="slide" id="instructions">
      <p id='logo-text'>Is it the same kind?</p>
      <p class="block-text"> In this experiment, you will first study at a set of example objects. You will then be asked to decide if a new objects is of the same kind as the examples. </p>
        
      <button type="button" onclick="this.blur(); experiment.next()">Start</button>

      <p class="block-text" id="legal"> Legal information: By answering the following questions, you are participating in a study being performed by cognitive scientists in the Stanford Department of Psychology. If you have questions about this research, please contact Ranjitha Kumar at ranju@stanford.edu, or Noah Goodman at ngoodman@stanford.edu. You must be at least 18 years old to participate. Your participation in this research is voluntary. You may decline to answer any or all of the following questions. You may decline further participation, at any time, without adverse consequences. Your anonymity is assured; the researchers who have requested your participation will not receive any personal information about you. </p>
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
          
        function addImage($filePath, $valign)
        {
            echo "<td valign=" . $valign . " align=center>";
            echo "<img src=" . $filePath . "></td>";
        }
        
        function addLikertRow($scale,$name)
        {
            echo '<td valign=middle>';
            echo "<form name=\"myform\">";
            echo '<table>';
            
            echo '<tr>';
            for($c=1;$c<=$scale;$c++) {
                echo "<td align=center style=width:100px><input type=\"radio\" name=\"r_" . $name . "\" value=" . $c . " onclick=\"experiment.next()\">" . $c . "</td>"; 
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
          
        function displayExamples($exemplars, $numRows, $numCols)
        {
            // display examples
            echo "<table style=\"border:5px solid gray;\">";
            for($r=0;$r<$numRows;$r++) {
                echo '<tr>';
                for($c=0;$c<$numCols;$c++) {
                    $index = $r*$numCols+$c;
                    if ($index < count($exemplars)){
                        addImage($exemplars[$index], "bottom");
                    }
                }
                echo '</tr>';
            }
            echo '</table>';
        }
          
        function displayTask($task, $currNumTask)
        {
            echo "<p>How <b>likely</b> is it that the following object is the same kind as the ones in the box above?</p>";
            
            // display task
            echo '<table>';
                echo '<tr>';
                addImage($task, "middle");
                addLikertRow(7,$currNumTask);
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
           
            echo "<div class=\"slide\" id=\"stage0\">";
            echo "<p>The below objects are examples of a particular kind that have been generated for a video game. Please study them carefully before going on.</p>";
            displayExamples($exemplars, 2, 5);
            echo "<button type=\"button\" onclick=\"this.blur(); experiment.next()\">Next</button>";
            echo "</div>";
            
            for ($j=0; $j<$numTasks; $j++)
            {
                $globalTaskID = $i*$numTasks + $j + 1;
                echo "<div class=\"slide\" id=\"stage" . $globalTaskID . "\">";
                echo "<p>Task <span id=\"currTaskNum\">" . $globalTaskID . "</span> of <span id=\"numTasks\">" . ($numExamples*$numTasks) . "</span>.</p>";

                echo "<p>The below objects are examples of a particular kind that have been generated for a video game. Please study them carefully before going on.</p>";
                
                displayExamples($exemplars, 2, 5);
                displayTask($tasks[$j], $globalTaskID);

                echo "<span id=\"example" . $globalTaskID . "\" class=\"scriptVar\">" . end(explode("/",$examples[$i])) . "</span>";
                echo "<span id=\"condition" . $globalTaskID . "\" class=\"scriptVar\">" . end(explode("-",end(explode("/",$tasks[$j],-1)))) . "</span>";
                echo "<span id=\"task" . $globalTaskID ."\" class=\"scriptVar\">" . end(explode("/",$tasks[$j])) . "</span>";
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
