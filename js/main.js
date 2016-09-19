var myDataRef = new Firebase('https://password-protection.firebaseio.com/');


function testFirebaseSet() {
    var usersRef = myDataRef.child("users");
    
    var username = 'tricorius';
    var email = 'tricorius@knightsoftheoldcode.com';
    
    var userRef = usersRef.child(username);
    
    userRef.set({email: email});
};

testFirebaseSet();
