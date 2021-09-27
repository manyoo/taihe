const three = require('three')

exports.mkRaycaster = _ => {
    return new three.Raycaster()
}

exports.jssetFromCamera = rc => v => camera => _ => {
    rc.setFromCamera(v, camera)
}

exports.jsintersectObject = r => obj => rec => _ => {
    return r.intersectObject(obj, rec)
}

exports.distance = obj => {
    return obj.distance
}

exports.point = obj => {
    return obj.point
}

exports.face = obj => {
    return obj.face
}

exports.object = obj => {
    return obj.object
}