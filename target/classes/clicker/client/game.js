let username = "";
const socket = io.connect("http://localhost:8080", {transports: ['websocket']});

setupSocket();

function setupSocket() {

    socket.on('initialize', function (event) {
        const gameSetup = JSON.parse(event);

        let gameHTML = "<br/><h1 id=\"displayCurrency\">0</h1>";
        gameHTML += "<button style=\"width:200px;height:100px;font-size:30px\"  onclick=\"clickCurrency();\">" + gameSetup['currency'] + "</button>";
        gameHTML += '<br/><br/><br/>';

        gameHTML += '<table border="single" cellspacing="0" cellpadding="5">\n' +
            '    <tr>\n' +
            '        <th></th>\n' +
            '        <th>Name</th>\n' +
            '        <th>owned</th>\n' +
            '        <th>cost</th>\n' +
            '        <th>income per click</th>\n' +
            '        <th>income per second</th>\n' +
            '    </tr>';

        for (const equipment of gameSetup['equipment']) {
            gameHTML += '<tr>\n' +
                '        <td>\n' +
                '            <button onclick="buyEquipment(\'' + equipment['id'] + '\')">Buy</button>\n' +
                '        </td>\n' +
                '        <td>' + equipment['name'] + '</td>\n' +
                '        <td id="' + equipment['id'] + '_owned">0</td>\n' +
                '        <td id="' + equipment['id'] + '_cost">' + equipment['initialCost'] + '</td>\n' +
                '        <td>' + equipment['incomePerClick'] + '</td>\n' +
                '        <td>' + equipment['incomePerSecond'] + '</td>\n' +
                '    </tr>';
        }

        gameHTML += '</table><br/>';

        document.getElementById('game').innerHTML = gameHTML;
    });


    socket.on('gameState', function (event) {
        const gameState = JSON.parse(event);
        document.getElementById("displayCurrency").innerHTML = gameState['currency'].toFixed(0);
        const allEquipment = gameState['equipment'];
        for (const equipment of allEquipment) {
            const eqId = equipment['id'];
            document.getElementById(eqId + '_owned').innerHTML = equipment['numberOwned'];
            document.getElementById(eqId + '_cost').innerHTML = equipment['cost'].toFixed(0);
        }
    });
}


function submitUsername() {
    const enteredUsername = document.getElementById("username").value;
    if (enteredUsername !== "") {
        username = enteredUsername;
        socket.emit("startGame", enteredUsername);
    }
}


function clickCurrency() {
    socket.emit("click");
}


function buyEquipment(equipmentID) {
    socket.emit("buy", equipmentID);
}
