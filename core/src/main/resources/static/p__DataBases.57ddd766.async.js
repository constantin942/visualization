(self.webpackChunkant_design_pro=self.webpackChunkant_design_pro||[]).push([[383],{12265:function(U){U.exports={databases:"databases___35H4X","detail-wrap":"detail-wrap___1IA0t","tops-wrap":"tops-wrap___2AQns",tops:"tops___2lHCZ"}},59498:function(U,D,e){"use strict";e.r(D),e.d(D,{default:function(){return te}});var f=e(11849),p=e(57663),C=e(71577),u=e(34792),v=e(48086),a=e(86582),n=e(2824),t=e(67294),s=e(47369),l=e(13062),h=e(71230),b=e(89032),T=e(15746),x=e(39004),i=e(85893),L=function(m){return(0,t.useEffect)(function(){var _=new x.kL({container:"dataActionPie".concat(m.id),autoFit:!0,height:250});_.data(m.types),_.coordinate("theta",{radius:.75}),_.tooltip({showMarkers:!0}),_.interval().adjust("stack").position("value").color("type",["#063d8a","#1770d6","#47abfc","#38c060"]).style({opacity:.4}).state({active:{style:function(R){var B=R.shape;return{matrix:x.Zr.zoom(B,1.1)}}}}).label("type",function(w){var R=w==="select"?1:.5;return{offset:-30,style:{opacity:R,fill:"white",fontSize:12,shadowBlur:2,shadowColor:"rgba(0, 0, 0, .45)"},content:function(Z){return Z.type+`
`+Z.value+"\u6B21"}}}),_.interaction("element-single-selected"),_.render()},[]),(0,i.jsx)("div",{id:"dataActionPie"+m.id})},A=L,o=function(m){return(0,t.useEffect)(function(){var _=new x.kL({container:"dataRecentlyLine".concat(m.id),autoFit:!0,height:200});_.data(m.ranges),_.scale({value:{min:0,nice:!1},year:{range:[0,1]}}),_.tooltip({}),_.axis("value",{label:{}}),_.area().position("year*value"),_.line().position("year*value"),_.render()},[]),(0,i.jsx)("div",{id:"dataRecentlyLine"+m.id})},d=o,g=function(m){return(0,i.jsx)(h.Z,{className:m.style["detail-wrap"],gutter:20,children:(0,i.jsx)(T.Z,{span:24,children:(0,i.jsxs)(h.Z,{children:[(0,i.jsxs)(T.Z,{span:12,className:"text-c",children:[(0,i.jsx)("h3",{children:"\u64CD\u4F5C\u884C\u4E3A\u5206\u5E03"}),(0,i.jsx)(A,{types:m.types,id:m.id})]}),(0,i.jsx)(T.Z,{span:12,className:"text-c",children:(0,i.jsxs)("div",{className:"text-c mt-15 mb-20",children:[(0,i.jsx)("h3",{children:"\u8FD1\u4E03\u5929\u8BBF\u95EE\u6570\u636E\u7EDF\u8BA1"}),(0,i.jsx)(d,{ranges:m.ranges,id:m.id})]})})]})})})},y=g,r=e(12265),H=e.n(r),Q=e(66572),I=e(24480),F=e(6650),q=e(84674),ee=function(){var m=(0,t.useState)([]),_=(0,n.Z)(m,2),w=_[0],R=_[1],B=(0,t.useState)(""),Z=(0,n.Z)(B,2),ae=Z[0],ne=Z[1],se=(0,t.useState)(0),G=(0,n.Z)(se,2),re=G[0],ue=G[1],ie=(0,t.useState)({pageSize:20,pageNo:1}),$=(0,n.Z)(ie,2),W=$[0],oe=$[1],le=(0,t.useState)({types:[],ranges:[]}),z=(0,n.Z)(le,2),de=z[0],ce=z[1],_e=[{title:"\u6570\u636E\u5E93\u8868",dataIndex:"tableName",key:"tableName",align:"center",render:function(O,c){var S="-";if(c.tableNameDesc)S=c.tableNameDesc;else{var E=c.tableName.split("#");S=E[E.length-1]}return(0,i.jsx)("div",{style:{wordWrap:"break-word",wordBreak:"break-word"},children:S})}},{title:"\u8BBF\u95EE\u6B21\u6570",dataIndex:"visitedCount",key:"visitedCount",align:"center",search:!1},{title:"\u5E38\u8BBF\u95EE\u4EBA\u5458",key:"usualVisitedUser",dataIndex:"usualVisitedUser",align:"center",search:!1},{title:"\u6700\u540E\u8BBF\u95EE\u65F6\u95F4",key:"lastVisitedDate",dataIndex:"lastVisitedDate",valueType:"date",hideInSetting:!0,search:!1,align:"center"},{title:"\u64CD\u4F5C",key:"action",align:"center",search:!1,render:function(O,c){return[(0,i.jsx)(C.Z,{type:"link",size:"small",onClick:function(){if(c.visitedCount!==0){var E=I.Z.getRangeStartAndEndTime(7,"YYYY-mm-dd HH:MM:SS"),k=I.Z.getRangeTime(7,"YYYY-mm-dd");(0,F.Xd)({msTableName:c.tableName,startTime:E[0],endTime:E[1],tableName:c.tableName}).then(function(P){var K=P.data;if(K&&K.length>0){var me=K.map(function(j,N){return{year:k[N],value:j}}),V=(0,a.Z)(q.q);(0,F.et)({tableName:c.tableName}).then(function(j){j.data&&j.data.length>0&&V.forEach(function(N){j.data.forEach(function(fe){var X=JSON.parse(fe),ve=X.dbType,pe=X.dbTypeTimes;N.type===ve&&(N.value=parseInt(pe))})})}),ce({ranges:me,types:V}),setTimeout(function(){ne(c.id)},400)}})}else v.ZP.info("".concat(c.tableName,"\u8868\u8BBF\u88AB\u95EE\u6B21\u6570\u4E3A0\uFF0C\u6682\u65E0\u8BE6\u60C5\u6570\u636E")).then()},children:"\u8BE6\u60C5"},"action")]}}],J=function(O){(0,Q.m)(O).then(function(c){var S=c.data;if(S){var E=JSON.parse(S);ue(E.total);var k=JSON.parse(E.rows);R(k.map(function(P){return P.id=I.Z.uuid(),P}))}})};return(0,t.useEffect)(function(){J(W)},[]),(0,i.jsx)("div",{className:H().databases,children:(0,i.jsx)(s.ZP,{columns:_e,rowKey:"id",pagination:{total:re,pageSize:W.pageSize,current:W.pageNo,onChange:function(O,c){oe({pageNo:O,pageSize:c}),J({pageNo:O,pageSize:c})}},expandable:{expandedRowRender:function(O,c){return(0,t.createElement)(y,(0,f.Z)((0,f.Z)({},de),{},{id:c,key:c,style:H()}))},expandIcon:function(){return!1},expandedRowKeys:[ae]},dataSource:w})})},te=ee},84674:function(U,D,e){"use strict";e.d(D,{q:function(){return f},Q:function(){return p}});var f=[{type:"insert",value:0},{type:"delete",value:0},{type:"update",value:0},{type:"select",value:0}],p=[{type:"\u4E0A\u5348",value:0},{type:"\u4E0B\u5348",value:0},{type:"\u665A\u4E0A",value:0}]},66572:function(U,D,e){"use strict";e.d(D,{m:function(){return a}});var f=e(11849),p=e(3182),C=e(94043),u=e.n(C),v=e(21704);function a(t,s){return n.apply(this,arguments)}function n(){return n=(0,p.Z)(u().mark(function t(s,l){return u().wrap(function(b){for(;;)switch(b.prev=b.next){case 0:return b.abrupt("return",(0,v.WY)("/api/skyflying/getCoarseCountsOfTableName",(0,f.Z)({method:"GET",params:s},l||{})));case 1:case"end":return b.stop()}},t)})),n.apply(this,arguments)}},6650:function(U,D,e){"use strict";e.d(D,{F$:function(){return a},Xd:function(){return t},et:function(){return b},Bx:function(){return x},wA:function(){return L}});var f=e(11849),p=e(3182),C=e(94043),u=e.n(C),v=e(32773);function a(o){return n.apply(this,arguments)}function n(){return n=(0,p.Z)(u().mark(function o(d){return u().wrap(function(y){for(;;)switch(y.prev=y.next){case 0:return y.abrupt("return",(0,v.WY)("/api/skyflying/getCoarseCountsOfUser",(0,f.Z)({method:"GET"},d||{})));case 1:case"end":return y.stop()}},o)})),n.apply(this,arguments)}function t(o,d){return s.apply(this,arguments)}function s(){return s=(0,p.Z)(u().mark(function o(d,g){return u().wrap(function(r){for(;;)switch(r.prev=r.next){case 0:return r.abrupt("return",(0,v.WY)("/api/skyflying/getCountsOfUserRecentSevenDays",(0,f.Z)({method:"GET",params:d},g||{})));case 1:case"end":return r.stop()}},o)})),s.apply(this,arguments)}function l(o,d){return h.apply(this,arguments)}function h(){return h=_asyncToGenerator(_regeneratorRuntime.mark(function o(d,g){return _regeneratorRuntime.wrap(function(r){for(;;)switch(r.prev=r.next){case 0:return r.abrupt("return",request("/api/skyflying/getCountsOfAllRecentSevenDays",_objectSpread({method:"GET",params:d},g||{})));case 1:case"end":return r.stop()}},o)})),h.apply(this,arguments)}function b(o,d){return T.apply(this,arguments)}function T(){return T=(0,p.Z)(u().mark(function o(d,g){return u().wrap(function(r){for(;;)switch(r.prev=r.next){case 0:return r.abrupt("return",(0,v.WY)("/api/skyflying/getCountsOfUser",(0,f.Z)({method:"GET",params:d},g||{})));case 1:case"end":return r.stop()}},o)})),T.apply(this,arguments)}function x(o,d){return i.apply(this,arguments)}function i(){return i=(0,p.Z)(u().mark(function o(d,g){return u().wrap(function(r){for(;;)switch(r.prev=r.next){case 0:return r.abrupt("return",(0,v.WY)("/api/skyflying/getUserUsualAndUnusualData",(0,f.Z)({method:"GET",params:d},g||{})));case 1:case"end":return r.stop()}},o)})),i.apply(this,arguments)}function L(o,d){return A.apply(this,arguments)}function A(){return A=(0,p.Z)(u().mark(function o(d,g){return u().wrap(function(r){for(;;)switch(r.prev=r.next){case 0:return r.abrupt("return",(0,v.WY)("/api/skyflying/getCoarseCountsOfOneUser",(0,f.Z)({method:"GET",params:d||{}},g||{})));case 1:case"end":return r.stop()}},o)})),A.apply(this,arguments)}},24480:function(U,D,e){"use strict";e.d(D,{Z:function(){return C}});var f=e(69610),p=e(54941),C=function(){function u(){(0,f.Z)(this,u)}return(0,p.Z)(u,null,[{key:"timeStampToTime",value:function(a){var n=new Date(parseInt(a));return n.toJSON().substring(0,10).replace("T","")}},{key:"getLocalTime",value:function(a){var n=arguments.length>1&&arguments[1]!==void 0?arguments[1]:"YYYY-mm-dd HH:MM:SS",t,s={"Y+":a.getFullYear().toString(),"m+":(a.getMonth()+1).toString(),"d+":a.getDate().toString(),"H+":a.getHours().toString(),"M+":a.getMinutes().toString(),"S+":a.getSeconds().toString()},l=n;for(var h in s)t=new RegExp("("+h+")").exec(n),t&&(l=l.replace(t[1],t[1].length==1?s[h]:s[h].padStart(t[1].length,"0")));return l}},{key:"uuid",value:function(){for(var a=[],n="0123456789abcdef",t=0;t<36;t++)a[t]=n.substr(Math.floor(Math.random()*16),1);return a[14]="4",a[19]=n.substr(a[19]&3|8,1),a[8]=a[13]=a[18]=a[23]="-",a.join("").replace(new RegExp(/(-)/g),"")}},{key:"getRangeTime",value:function(a,n){for(var t=[],s=a-1;s>=0;s--){var l=void 0;s===a-1?l=new Date(Date.now()-s*24*60*60*1e3).setHours(0,0,1,0):l=new Date(Date.now()-s*24*60*60*1e3).setHours(23,59,59,999),n?t.push(u.getLocalTime(new Date(l),n)):t.push(l)}return t}},{key:"getRangeStartAndEndTime",value:function(a,n){for(var t=[],s=a-1;s>=0;s--){var l=void 0;s===a-1?l=new Date(Date.now()-s*24*60*60*1e3).setHours(0,0,0,0):s===0?l=new Date(Date.now()-(s-1)*24*60*60*1e3).setHours(0,0,0,0):l=new Date(Date.now()-s*24*60*60*1e3).setHours(23,59,59,999),n?t.push(u.getLocalTime(new Date(l),n)):t.push(l)}return[t[0],t[a-1]]}}]),u}();C.arrayDeDuplication=function(u){return Array.from(new Set(u))}}}]);