(self.webpackChunkant_design_pro=self.webpackChunkant_design_pro||[]).push([[383],{12265:function(B){B.exports={databases:"databases___35H4X","detail-wrap":"detail-wrap___1IA0t","tops-wrap":"tops-wrap___2AQns",tops:"tops___2lHCZ"}},59498:function(B,_,e){"use strict";e.r(_),e.d(_,{default:function(){return X}});var d=e(11849),f=e(57663),T=e(71577),i=e(34792),v=e(48086),U=e(86582),c=e(2824),l=e(67294),g=e(83539),R=e(13062),h=e(71230),y=e(89032),C=e(15746),j=e(69886),s=e(85893),N=function(r){return(0,l.useEffect)(function(){var m=new j.by("dataActionPie".concat(r.id),{autoFit:!0,height:250,appendPadding:10,data:r.types,angleField:"value",colorField:"type",radius:.75,legend:{position:"bottom"},label:{type:"spider",labelHeight:28,content:`{name}
{percentage}`},interactions:[{type:"element-selected"},{type:"element-active"}]});m.update({theme:{styleSheet:{brandColor:"#025DF4",paletteQualitative10:["#025DF4","#DB6BCF","#2498D1","#BBBDE6","#4045B2","#21A97A","#FF745A","#007E99","#FFA8A8","#2391FF"],paletteQualitative20:["#025DF4","#DB6BCF","#2498D1","#BBBDE6","#4045B2","#21A97A","#FF745A","#007E99","#FFA8A8","#2391FF","#FFC328","#A0DC2C","#946DFF","#626681","#EB4185","#CD8150","#36BCCB","#327039","#803488","#83BC99"]}}}),m.render()},[r.id,r.types]),(0,s.jsx)("div",{id:"dataActionPie"+r.id})},A=N,M=e(39004),S=function(r){return(0,l.useEffect)(function(){var m=new M.kL({container:"dataRecentlyLine".concat(r.id),autoFit:!0,height:200});m.data(r.ranges),m.scale({value:{min:0,nice:!1},year:{range:[0,1]}}),m.tooltip({}),m.axis("value",{label:{}}),m.area().position("year*value"),m.line().position("year*value"),m.render()},[r.id,r.ranges]),(0,s.jsx)("div",{id:"dataRecentlyLine"+r.id})},a=S,n=function(r){return(0,s.jsx)(h.Z,{className:r.style["detail-wrap"],gutter:20,children:(0,s.jsx)(C.Z,{span:24,children:(0,s.jsxs)(h.Z,{children:[(0,s.jsxs)(C.Z,{span:12,className:"text-c",children:[(0,s.jsx)("h3",{children:"\u64CD\u4F5C\u884C\u4E3A\u5206\u5E03"}),(0,s.jsx)(A,{types:r.types,id:r.id})]}),(0,s.jsx)(C.Z,{span:12,className:"text-c",children:(0,s.jsxs)("div",{className:"text-c mt-15 mb-20",children:[(0,s.jsx)("h3",{children:"\u8FD1\u4E03\u5929\u8BBF\u95EE\u6570\u636E\u7EDF\u8BA1"}),(0,s.jsx)(a,{ranges:r.ranges,id:r.id})]})})]})})})},o=n,p=e(12265),t=e.n(p),J=e(66572),I=e(24480),Y=e(6650),Q=e(84674),V=function(){var r=(0,l.useState)([]),m=(0,c.Z)(r,2),q=m[0],ee=m[1],te=(0,l.useState)(""),k=(0,c.Z)(te,2),ae=k[0],ne=k[1],se=(0,l.useState)(0),K=(0,c.Z)(se,2),re=K[0],ue=K[1],ie=(0,l.useState)({pageSize:10,pageNo:1}),G=(0,c.Z)(ie,2),Z=G[0],le=G[1],oe=(0,l.useState)({types:[],ranges:[]}),$=(0,c.Z)(oe,2),de=$[0],ce=$[1],me=[{title:"\u6570\u636E\u5E93\u8868",dataIndex:"tableName",key:"tableName",align:"center",render:function(D,u){var E="-";if(u.tableNameDesc)E=u.tableNameDesc;else{var b=u.tableName.split("#");E=b[b.length-1]}return(0,s.jsx)("div",{style:{wordWrap:"break-word",wordBreak:"break-word"},children:E})}},{title:"\u88AB\u8BBF\u95EE\u6B21\u6570",dataIndex:"visitedCount",key:"visitedCount",align:"center",search:!1},{title:"\u5E38\u8BBF\u95EE\u4EBA\u5458",key:"usualVisitedUser",dataIndex:"usualVisitedUser",align:"center",search:!1},{title:"\u6700\u540E\u8BBF\u95EE\u65F6\u95F4",key:"lastVisitedDate",dataIndex:"lastVisitedDate",valueType:"date",hideInSetting:!0,search:!1,align:"center"},{title:"\u64CD\u4F5C",key:"action",align:"center",search:!1,render:function(D,u){return[(0,s.jsx)(T.Z,{type:"link",size:"small",onClick:function(){if(u.visitedCount!==0){var b=I.Z.getRangeStartAndEndTime(7,"YYYY-mm-dd HH:MM:SS"),x=I.Z.getRangeTime(7,"YYYY-mm-dd");(0,Y.Xd)({msTableName:u.tableName,startTime:b[0],endTime:b[1],tableName:u.tableName}).then(function(pe){var L=pe.data;if(L&&L.length>0){var fe=L.map(function(F,P){return{year:x[P],value:F}}),z=(0,U.Z)(Q.q);(0,Y.et)({tableName:u.tableName}).then(function(F){F.data&&F.data.length>0&&z.forEach(function(P){F.data.forEach(function(ve){var H=JSON.parse(ve),_e=H.dbType,ge=H.dbTypeTimes;P.type===_e&&(P.value=parseInt(ge))})})}),ce({ranges:fe,types:z}),setTimeout(function(){ne(u.id)},400)}})}else v.ZP.info("".concat(u.tableName,"\u8868\u8BBF\u88AB\u95EE\u6B21\u6570\u4E3A0\uFF0C\u6682\u65E0\u8BE6\u60C5\u6570\u636E")).then()},children:"\u8BE6\u60C5"},"action")]}}],w=(0,l.useCallback)(function(O){(0,J.m)(O).then(function(D){var u=D.data;if(u){var E=JSON.parse(u);ue(E.total);var b=JSON.parse(E.rows);ee(b.map(function(x){return x.id=I.Z.uuid(),x}))}})},[]);return(0,l.useEffect)(function(){w(Z)},[w,Z]),(0,s.jsx)("div",{className:t().databases,children:(0,s.jsx)(g.ZP,{columns:me,rowKey:"id",pagination:{showSizeChanger:!1,total:re,pageSize:Z.pageSize,current:Z.pageNo,onChange:function(D,u){le({pageNo:D,pageSize:u}),w({pageNo:D,pageSize:u})}},expandable:{expandedRowRender:function(D,u){return(0,l.createElement)(o,(0,d.Z)((0,d.Z)({},de),{},{id:u,key:u,style:t()}))},expandIcon:function(){return!1},expandedRowKeys:[ae]},dataSource:q})})},X=V},84674:function(B,_,e){"use strict";e.d(_,{q:function(){return d},Q:function(){return f}});var d=[{type:"insert",value:0},{type:"delete",value:0},{type:"update",value:0},{type:"select",value:0}],f=[{type:"\u4E0A\u5348",value:0},{type:"\u4E0B\u5348",value:0},{type:"\u665A\u4E0A",value:0}]},66572:function(B,_,e){"use strict";e.d(_,{m:function(){return U}});var d=e(11849),f=e(3182),T=e(94043),i=e.n(T),v=e(25377);function U(l,g){return c.apply(this,arguments)}function c(){return c=(0,f.Z)(i().mark(function l(g,R){return i().wrap(function(y){for(;;)switch(y.prev=y.next){case 0:return y.abrupt("return",(0,v.WY)("/api/skyflying/getCoarseCountsOfTableName",(0,d.Z)({method:"GET",params:g},R||{})));case 1:case"end":return y.stop()}},l)})),c.apply(this,arguments)}},6650:function(B,_,e){"use strict";e.d(_,{F$:function(){return U},Xd:function(){return l},et:function(){return y},FP:function(){return j},Bx:function(){return N},wA:function(){return M}});var d=e(11849),f=e(3182),T=e(94043),i=e.n(T),v=e(25377);function U(a){return c.apply(this,arguments)}function c(){return c=(0,f.Z)(i().mark(function a(n){return i().wrap(function(p){for(;;)switch(p.prev=p.next){case 0:return p.abrupt("return",(0,v.WY)("/api/skyflying/getCoarseCountsOfUser",(0,d.Z)({method:"GET"},n||{})));case 1:case"end":return p.stop()}},a)})),c.apply(this,arguments)}function l(a,n){return g.apply(this,arguments)}function g(){return g=(0,f.Z)(i().mark(function a(n,o){return i().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,v.WY)("/api/skyflying/getCountsOfUserRecentSevenDays",(0,d.Z)({method:"GET",params:n},o||{})));case 1:case"end":return t.stop()}},a)})),g.apply(this,arguments)}function R(a,n){return h.apply(this,arguments)}function h(){return h=_asyncToGenerator(_regeneratorRuntime.mark(function a(n,o){return _regeneratorRuntime.wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",request("/api/skyflying/getCountsOfAllRecentSevenDays",_objectSpread({method:"GET",params:n},o||{})));case 1:case"end":return t.stop()}},a)})),h.apply(this,arguments)}function y(a,n){return C.apply(this,arguments)}function C(){return C=(0,f.Z)(i().mark(function a(n,o){return i().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,v.WY)("/api/skyflying/getCountsOfUser",(0,d.Z)({method:"GET",params:n},o||{})));case 1:case"end":return t.stop()}},a)})),C.apply(this,arguments)}function j(a,n){return s.apply(this,arguments)}function s(){return s=(0,f.Z)(i().mark(function a(n,o){return i().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,v.WY)("/api/skyflying/getUserOperationTypeCount",(0,d.Z)({method:"GET",params:n},o||{})));case 1:case"end":return t.stop()}},a)})),s.apply(this,arguments)}function N(a,n){return A.apply(this,arguments)}function A(){return A=(0,f.Z)(i().mark(function a(n,o){return i().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,v.WY)("/api/skyflying/getUserUsualAndUnusualData",(0,d.Z)({method:"GET",params:n},o||{})));case 1:case"end":return t.stop()}},a)})),A.apply(this,arguments)}function M(a,n){return S.apply(this,arguments)}function S(){return S=(0,f.Z)(i().mark(function a(n,o){return i().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,v.WY)("/api/skyflying/getCoarseCountsOfOneUser",(0,d.Z)({method:"GET",params:n||{}},o||{})));case 1:case"end":return t.stop()}},a)})),S.apply(this,arguments)}}}]);