<script type="text/javascript" src="html2canvas-master/dist/html2canvas.js"></script>

html2canvas(document.body).then(function(canvas) {
 document.body.appendChild(canvas);
});

html2canvas(document.getElementById('container')).then(function(canvas) {
 document.getElementById("image").src= canvas.toDataURL();
});

<!doctype html>
<html>
 <head>
  <script type="text/javascript" src="html2canvas-master/dist/html2canvas.js"></script>
 </head>
 <body>
  <h1>Take screenshot of webpage with html2canvas</h1>
  <div class="container" id='container' >
   <img src='images/image1.jpg' width='100' height='100'>
   <img src='images/image2.jpg' width='100' height='100'>
   <img src='images/image3.jpg' width='100' height='100'>
  </div>
  <input type='button' id='but_screenshot' value='Take screenshot' onclick='screenshot();'><br/>

  <!-- Script -->
  <script type='text/javascript'>
  function screenshot(){
    html2canvas(document.body).then(function(canvas) {
    document.body.appendChild(canvas);
   });
  }
  </script>

 </body>
</html>