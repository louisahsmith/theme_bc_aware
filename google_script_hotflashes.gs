var hotflashsheet = SpreadsheetApp.openById("yoursheetnumber").getSheetByName('Sheet1');

function doGet(e) {

  var datetime = Utilities.formatDate(new Date(), "GMT-4", "yyyy-MM-dd'T'HH:mm:ss");

  hotflashsheet.appendRow([datetime]);

}
