import ChromeMessage = require("../ChromeMessage");

var cm = new ChromeMessage("tests.ChromeMessage.cm");

function asyncResponse () {

    cm.addListener((content, sendResponse) => {

        // send async response after 1 second timeout
        setTimeout(() => {
            try {
                sendResponse("success");
            } catch (e) {
                console.error("tests.ChromeMessage.asyncResponse failed:", e);
            }
        }, 1000);

        return true; // async
    });

    cm.call((res : string) => {
        console.log(res);
    });
}

export function run () {
    asyncResponse();
}
