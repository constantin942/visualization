(self.webpackChunkant_design_pro=self.webpackChunkant_design_pro||[]).push([[383],{12265:function(T){T.exports={databases:"databases___35H4X","detail-wrap":"detail-wrap___1IA0t","tops-wrap":"tops-wrap___2AQns",tops:"tops___2lHCZ"}},59498:function(T,D,e){"use strict";e.r(D),e.d(D,{default:function(){return ee}});var m=e(11849),p=e(57663),C=e(71577),u=e(34792),f=e(48086),a=e(86582),n=e(2824),t=e(67294),s=e(47369),l=e(13062),h=e(71230),E=e(89032),b=e(15746),U=e(39004),i=e(85893),B=function(_){return(0,t.useEffect)(function(){var c=new U.kL({container:"dataActionPie".concat(_.id),autoFit:!0,height:250});c.data(_.types),c.coordinate("theta",{radius:.75}),c.tooltip({showMarkers:!0}),c.interval().adjust("stack").position("value").color("type",["#063d8a","#1770d6","#47abfc","#38c060"]).style({opacity:.4}).state({active:{style:function(A){var M=A.shape;return{matrix:U.Zr.zoom(M,1.1)}}}}).label("type",function(Z){var A=Z==="select"?1:.5;return{offset:-30,style:{opacity:A,fill:"white",fontSize:12,shadowBlur:2,shadowColor:"rgba(0, 0, 0, .45)"},content:function(x){return x.type+`
`+x.value+"\u6B21"}}}),c.interaction("element-single-selected"),c.render()},[]),(0,i.jsx)("div",{id:"dataActionPie"+_.id})},S=B,o=function(_){return(0,t.useEffect)(function(){var c=new U.kL({container:"dataRecentlyLine".concat(_.id),autoFit:!0,height:200});c.data(_.ranges),c.scale({value:{min:0,nice:!1},year:{range:[0,1]}}),c.tooltip({}),c.axis("value",{label:{}}),c.area().position("year*value"),c.line().position("year*value"),c.render()},[]),(0,i.jsx)("div",{id:"dataRecentlyLine"+_.id})},d=o,g=function(_){return(0,i.jsx)(h.Z,{className:_.style["detail-wrap"],gutter:20,children:(0,i.jsx)(b.Z,{span:24,children:(0,i.jsxs)(h.Z,{children:[(0,i.jsxs)(b.Z,{span:12,className:"text-c",children:[(0,i.jsx)("h3",{children:"\u64CD\u4F5C\u884C\u4E3A\u5206\u5E03"}),(0,i.jsx)(S,{types:_.types,id:_.id})]}),(0,i.jsx)(b.Z,{span:12,className:"text-c",children:(0,i.jsxs)("div",{className:"text-c mt-15 mb-20",children:[(0,i.jsx)("h3",{children:"\u8FD1\u4E03\u5929\u8BBF\u95EE\u6570\u636E\u7EDF\u8BA1"}),(0,i.jsx)(d,{ranges:_.ranges,id:_.id})]})})]})})})},y=g,r=e(12265),H=e.n(r),Q=e(66572),w=e(24480),F=e(6650),G=e(84674),q=function(){var _=(0,t.useState)([]),c=(0,n.Z)(_,2),Z=c[0],A=c[1],M=(0,t.useState)(""),x=(0,n.Z)(M,2),te=x[0],ae=x[1],ne=(0,t.useState)(0),$=(0,n.Z)(ne,2),se=$[0],re=$[1],ue=(0,t.useState)({pageSize:20,pageNo:1}),z=(0,n.Z)(ue,2),N=z[0],ie=z[1],oe=(0,t.useState)({types:[],ranges:[]}),V=(0,n.Z)(oe,2),le=V[0],de=V[1],ce=[{title:"\u6570\u636E\u5E93\u8868",dataIndex:"tableName",key:"tableName",align:"center"},{title:"\u8BBF\u95EE\u6B21\u6570",dataIndex:"visitedCount",key:"visitedCount",align:"center",search:!1},{title:"\u5E38\u8BBF\u95EE\u4EBA\u5458",key:"usualVisitedUser",dataIndex:"usualVisitedUser",align:"center",search:!1},{title:"\u6700\u540E\u8BBF\u95EE\u65F6\u95F4",key:"lastVisitedDate",dataIndex:"lastVisitedDate",valueType:"date",hideInSetting:!0,search:!1,align:"center"},{title:"\u64CD\u4F5C",key:"action",align:"center",search:!1,render:function(O,v){return[(0,i.jsx)(C.Z,{type:"link",size:"small",onClick:function(){if(v.visitedCount!==0){var R=w.Z.getRangeStartAndEndTime(7,"YYYY-mm-dd HH:MM:SS"),Y=w.Z.getRangeTime(7,"YYYY-mm-dd");(0,F.Xd)({msTableName:v.tableName,startTime:R[0],endTime:R[1]}).then(function(j){var W=j.data;if(W&&W.length>0){var _e=W.map(function(k,K){return{year:Y[K],value:k}}),X=(0,a.Z)(G.q);G.q.forEach(function(k,K){(0,F.et)({msTableName:v.tableName,dbType:k.type}).then(function(me){X[K].value=me.data})}),de({ranges:_e,types:X}),setTimeout(function(){ae(v.id)},400)}})}else f.ZP.info("".concat(v.tableName,"\u8868\u8BBF\u88AB\u95EE\u6B21\u6570\u4E3A0\uFF0C\u6682\u65E0\u8BE6\u60C5\u6570\u636E")).then()},children:"\u8BE6\u60C5"},"action")]}}],J=function(O){(0,Q.m)(O).then(function(v){var I=v.data;if(I){var R=JSON.parse(I);re(R.total);var Y=JSON.parse(R.rows);A(Y.map(function(j){return j.id=w.Z.uuid(),j}))}})};return(0,t.useEffect)(function(){J(N)},[]),(0,i.jsx)("div",{className:H().databases,children:(0,i.jsx)(s.ZP,{columns:ce,rowKey:"id",pagination:{total:se,pageSize:N.pageSize,current:N.pageNo,onChange:function(O,v){ie({pageNo:O,pageSize:v}),J({pageNo:O,pageSize:v})}},expandable:{expandedRowRender:function(O,v){return(0,t.createElement)(y,(0,m.Z)((0,m.Z)({},le),{},{id:v,key:v,style:H()}))},expandIcon:function(){return!1},expandedRowKeys:[te]},dataSource:Z})})},ee=q},84674:function(T,D,e){"use strict";e.d(D,{q:function(){return m},Q:function(){return p}});var m=[{type:"insert",value:0},{type:"delete",value:0},{type:"update",value:0},{type:"select",value:0}],p=[{type:"\u4E0A\u5348",value:0},{type:"\u4E0B\u5348",value:0},{type:"\u665A\u4E0A",value:0}]},66572:function(T,D,e){"use strict";e.d(D,{m:function(){return a}});var m=e(11849),p=e(3182),C=e(94043),u=e.n(C),f=e(21704);function a(t,s){return n.apply(this,arguments)}function n(){return n=(0,p.Z)(u().mark(function t(s,l){return u().wrap(function(E){for(;;)switch(E.prev=E.next){case 0:return E.abrupt("return",(0,f.WY)("/api/skyflying/getCoarseCountsOfTableName",(0,m.Z)({method:"GET",params:s},l||{})));case 1:case"end":return E.stop()}},t)})),n.apply(this,arguments)}},6650:function(T,D,e){"use strict";e.d(D,{F$:function(){return a},Xd:function(){return t},et:function(){return E},Bx:function(){return U},wA:function(){return B}});var m=e(11849),p=e(3182),C=e(94043),u=e.n(C),f=e(32773);function a(o){return n.apply(this,arguments)}function n(){return n=(0,p.Z)(u().mark(function o(d){return u().wrap(function(y){for(;;)switch(y.prev=y.next){case 0:return y.abrupt("return",(0,f.WY)("/api/skyflying/getCoarseCountsOfUser",(0,m.Z)({method:"GET"},d||{})));case 1:case"end":return y.stop()}},o)})),n.apply(this,arguments)}function t(o,d){return s.apply(this,arguments)}function s(){return s=(0,p.Z)(u().mark(function o(d,g){return u().wrap(function(r){for(;;)switch(r.prev=r.next){case 0:return r.abrupt("return",(0,f.WY)("/api/skyflying/getCountsOfUserRecentSevenDays",(0,m.Z)({method:"GET",params:d},g||{})));case 1:case"end":return r.stop()}},o)})),s.apply(this,arguments)}function l(o,d){return h.apply(this,arguments)}function h(){return h=_asyncToGenerator(_regeneratorRuntime.mark(function o(d,g){return _regeneratorRuntime.wrap(function(r){for(;;)switch(r.prev=r.next){case 0:return r.abrupt("return",request("/api/skyflying/getCountsOfAllRecentSevenDays",_objectSpread({method:"GET",params:d},g||{})));case 1:case"end":return r.stop()}},o)})),h.apply(this,arguments)}function E(o,d){return b.apply(this,arguments)}function b(){return b=(0,p.Z)(u().mark(function o(d,g){return u().wrap(function(r){for(;;)switch(r.prev=r.next){case 0:return r.abrupt("return",(0,f.WY)("/api/skyflying/getCountsOfUser",(0,m.Z)({method:"GET",params:d},g||{})));case 1:case"end":return r.stop()}},o)})),b.apply(this,arguments)}function U(o,d){return i.apply(this,arguments)}function i(){return i=(0,p.Z)(u().mark(function o(d,g){return u().wrap(function(r){for(;;)switch(r.prev=r.next){case 0:return r.abrupt("return",(0,f.WY)("/api/skyflying/getUserUsualAndUnusualData",(0,m.Z)({method:"GET",params:d},g||{})));case 1:case"end":return r.stop()}},o)})),i.apply(this,arguments)}function B(o,d){return S.apply(this,arguments)}function S(){return S=(0,p.Z)(u().mark(function o(d,g){return u().wrap(function(r){for(;;)switch(r.prev=r.next){case 0:return r.abrupt("return",(0,f.WY)("/api/skyflying/getCoarseCountsOfOneUser",(0,m.Z)({method:"GET",params:d||{}},g||{})));case 1:case"end":return r.stop()}},o)})),S.apply(this,arguments)}},24480:function(T,D,e){"use strict";e.d(D,{Z:function(){return C}});var m=e(69610),p=e(54941),C=function(){function u(){(0,m.Z)(this,u)}return(0,p.Z)(u,null,[{key:"timeStampToTime",value:function(a){var n=new Date(parseInt(a));return n.toJSON().substring(0,10).replace("T","")}},{key:"getLocalTime",value:function(a){var n=arguments.length>1&&arguments[1]!==void 0?arguments[1]:"YYYY-mm-dd HH:MM:SS",t,s={"Y+":a.getFullYear().toString(),"m+":(a.getMonth()+1).toString(),"d+":a.getDate().toString(),"H+":a.getHours().toString(),"M+":a.getMinutes().toString(),"S+":a.getSeconds().toString()},l=n;for(var h in s)t=new RegExp("("+h+")").exec(n),t&&(l=l.replace(t[1],t[1].length==1?s[h]:s[h].padStart(t[1].length,"0")));return l}},{key:"uuid",value:function(){for(var a=[],n="0123456789abcdef",t=0;t<36;t++)a[t]=n.substr(Math.floor(Math.random()*16),1);return a[14]="4",a[19]=n.substr(a[19]&3|8,1),a[8]=a[13]=a[18]=a[23]="-",a.join("").replace(new RegExp(/(-)/g),"")}},{key:"getRangeTime",value:function(a,n){for(var t=[],s=a-1;s>=0;s--){var l=void 0;s===a-1?l=new Date(Date.now()-s*24*60*60*1e3).setHours(0,0,1,0):l=new Date(Date.now()-s*24*60*60*1e3).setHours(23,59,59,999),n?t.push(u.getLocalTime(new Date(l),n)):t.push(l)}return t}},{key:"getRangeStartAndEndTime",value:function(a,n){for(var t=[],s=a-1;s>=0;s--){var l=void 0;s===a-1?l=new Date(Date.now()-s*24*60*60*1e3).setHours(0,0,0,0):s===0?l=new Date(Date.now()-(s-1)*24*60*60*1e3).setHours(0,0,0,0):l=new Date(Date.now()-s*24*60*60*1e3).setHours(23,59,59,999),n?t.push(u.getLocalTime(new Date(l),n)):t.push(l)}return[t[0],t[a-1]]}}]),u}();C.arrayDeDuplication=function(u){return Array.from(new Set(u))}}}]);
