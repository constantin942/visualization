(self.webpackChunkant_design_pro=self.webpackChunkant_design_pro||[]).push([[628],{26516:function(G){G.exports={overview:"overview___ZXPWZ",staticWrap:"staticWrap___2Invp",waringWrap:"waringWrap___AfQ25",bgW:"bgW___3rDWk",center:"center___21dU2",nowrap:"nowrap___spq6f"}},81135:function(G,M,e){"use strict";e.r(M),e.d(M,{default:function(){return we}});var T=e(57663),D=e(71577),I=e(58024),n=e(39144),_=e(13062),a=e(71230),l=e(95300),s=e(7277),i=e(89032),u=e(15746),b=e(71748),g=e(33860),S=e(3182),O=e(2824),j=e(94043),p=e.n(j),w=e(67294),ee=e(26516),k=e.n(ee),o=e(11849),m=e(46066),U=e(68628),t=e(85893),r=function(Z){var v={dots:!1,innerSlider:!0,slidesToShow:6,slidesToScroll:1,vertical:!0,verticalSwiping:!0,autoplay:!0,speed:2e3,autoplaySpeed:2e3,arrows:!1,cssEase:"linear"};return(0,t.jsxs)("div",{children:[(0,t.jsxs)("div",{children:[(0,t.jsx)(U.Z,{className:"fs-16 ib danger mr-10"}),(0,t.jsx)("h3",{className:"danger fs-16 ib",children:"\u544A\u8B66\u4FE1\u606F"})]}),(0,t.jsx)("div",{className:"of-hidden",children:(0,t.jsx)(m.Z,(0,o.Z)((0,o.Z)({},v),{},{children:Z.warningList.map(function(c){return(0,t.jsx)("div",{className:"mt-5 mb-5 text-c lh-2",children:c.alarmContent},c.id)})}))})]})},ce=r,de=e(69610),me=e(54941),pe=e(81306),ve=e(19809),F=e(69886),K=e(41694),fe=F.G2.InteractionAction,ge=F.G2.registerInteraction,he=F.G2.registerAction,_e=function(W){(0,pe.Z)(v,W);var Z=(0,ve.Z)(v);function v(){return(0,de.Z)(this,v),Z.apply(this,arguments)}return(0,me.Z)(v,[{key:"active",value:function(){var d=this,A=this.getView(),y=this.context.event;if(y.data){var f=y.data.items,N=A.geometries.filter(function(P){return P.type==="point"});(0,K.S6)(N,function(P){(0,K.S6)(P.elements,function($,V){var L=(0,K.cx)(f,function(X){return X.data===$.data})!==-1,H=$.shape.getChildren(),Y=(0,O.Z)(H,2),J=Y[0],ne=Y[1];L?(J.animate({r:10,opacity:.2},{duration:1800,easing:"easeLinear",repeat:!0}),ne.animate({r:6,opacity:.4},{duration:800,easing:"easeLinear",repeat:!0})):d.resetElementState($)})})}}},{key:"reset",value:function(){var d=this,A=this.getView(),y=A.geometries.filter(function(f){return f.type==="point"});(0,K.S6)(y,function(f){(0,K.S6)(f.elements,function(N){d.resetElementState(N)})})}},{key:"resetElementState",value:function(d){var A=d.shape.getChildren(),y=(0,O.Z)(A,2),f=y[0],N=y[1];f.stopAnimate(),N.stopAnimate();var P=f.get("attrs"),$=P.r,V=P.opacity;f.attr({r:$,opacity:V});var L=N.get("attrs"),H=L.r,Y=L.opacity;N.attr({r:H,opacity:Y})}},{key:"getView",value:function(){return this.context.view}}]),v}(fe),ye=function(Z){return(0,w.useEffect)(function(){F.G2.registerShape("point","custom-point",{draw:function(d,A){var y={x:d.x,y:d.y},f=A.addGroup();return f.addShape("circle",{name:"outer-point",attrs:{x:y.x,y:y.y,fill:d.color||"red",opacity:.5,r:6}}),f.addShape("circle",{name:"inner-point",attrs:{x:y.x,y:y.y,fill:d.color||"red",opacity:1,r:2}}),f}}),he("custom-marker-action",_e),ge("custom-marker-interaction",{start:[{trigger:"tooltip:show",action:"custom-marker-action:active"}],end:[{trigger:"tooltip:hide",action:"custom-marker-action:reset"}]});var v=new F.x1("RangeTimeData",{height:138,autoFit:!0,data:Z.data,xField:"year",yField:"value",label:{},point:{size:5,shape:"custom-point",style:{fill:"white",stroke:"#5B8FF9",lineWidth:2}},tooltip:{showMarkers:!1},state:{active:{style:{shadowBlur:4,stroke:"#000",fill:"red"}}},interactions:[{type:"custom-marker-interaction"}]});v.render()},[]),(0,t.jsx)("div",{id:"RangeTimeData"})},De=ye,se=e(47369),z=e(24480),Ce=e(6650),ie=e(32773);function Ee(W,Z){return te.apply(this,arguments)}function te(){return te=(0,S.Z)(p().mark(function W(Z,v){return p().wrap(function(d){for(;;)switch(d.prev=d.next){case 0:return d.abrupt("return",(0,ie.WY)("/api/skyflying/getCountsOfAllRecentSevenDays",(0,o.Z)({params:Z,method:"GET"},v||{})));case 1:case"end":return d.stop()}},W)})),te.apply(this,arguments)}function Se(W){return ae.apply(this,arguments)}function ae(){return ae=(0,S.Z)(p().mark(function W(Z){return p().wrap(function(c){for(;;)switch(c.prev=c.next){case 0:return c.abrupt("return",(0,ie.WY)("/api/skyflying/getOverviewOfSystem",(0,o.Z)({method:"GET"},Z||{})));case 1:case"end":return c.stop()}},W)})),ae.apply(this,arguments)}var Oe=e(66572),Te=e(2335),Ze=e(52778),xe=[{title:"\u7528\u6237\u540D",dataIndex:"userName",key:"name",align:"center"},{title:"\u8BBF\u95EE\u6B21\u6570",dataIndex:"visitedCount",key:"visitedCount",search:!1,align:"center"},{title:"\u5E38\u7528\u6570\u636E",dataIndex:"usualVisitedData",key:"usualVisitedData",align:"center",search:!1,render:function(Z,v){var c=JSON.parse(v.usualVisitedData),d=c.tableName,A=c.tableNameDesc,y=d.split(","),f="-";return A?f=A:y.length>1&&(f=y[y.length-1]),(0,t.jsx)("div",{style:{wordWrap:"break-word",wordBreak:"break-word"},children:f})}},{title:"\u6700\u540E\u8BBF\u95EE\u65F6\u95F4",dataIndex:"lastVisitedDate",key:"lastVisitedDate",search:!1,align:"center"}],Ue=[{title:"\u6570\u636E\u5E93\u8868",dataIndex:"tableName",key:"tableName",align:"center",render:function(Z,v){var c="-";if(v.tableNameDesc)c=v.tableNameDesc;else{var d=v.tableName.split("#");c=d[d.length-1]}return(0,t.jsx)("div",{style:{wordWrap:"break-word",wordBreak:"break-word"},children:c})}},{title:"\u8BBF\u95EE\u6B21\u6570",dataIndex:"visitedCount",key:"visitedCount",align:"center"},{title:"\u5E38\u8BBF\u95EE\u4EBA\u5458",key:"usualVisitedUser",dataIndex:"usualVisitedUser",align:"center"},{title:"\u6700\u540E\u8BBF\u95EE\u65F6\u95F4",key:"lastVisitedDate",dataIndex:"lastVisitedDate",valueType:"date",hideInSetting:!0,align:"center"}],be=function(Z){var v=(0,w.useState)(),c=(0,O.Z)(v,2),d=c[0],A=c[1],y=(0,w.useState)([]),f=(0,O.Z)(y,2),N=f[0],P=f[1],$=(0,w.useState)([]),V=(0,O.Z)($,2),L=V[0],H=V[1],Y=(0,w.useState)([]),J=(0,O.Z)(Y,2),ne=J[0],X=J[1],Ae=(0,w.useState)(0),ue=(0,O.Z)(Ae,2),je=ue[0],We=ue[1],Ne=(0,w.useState)({pageSize:10,pageNo:1}),Re=(0,O.Z)(Ne,1),re=Re[0],Be=(0,w.useState)({dbInstance:0,table:0,user:0,visitedInformation:0}),le=(0,O.Z)(Be,2),Q=le[0],Me=le[1];function Ie(){(0,Te.cD)({pageSize:100,pageNo:1}).then(function(R){var h=R.data;if(h){var C=JSON.parse(h);if(C){var E=JSON.parse(C.rows);X(E.map(function(x,B){return x.matchRuleId=B,x})),console.log(E)}else X([])}})}var ke=function(){(0,Ce.F$)().then(function(h){var C=h.data;if(C&&C.length>0){A(C.map(function(x){return x.id=z.Z.uuid(),x}));var E=[];C.forEach(function(x){E.push({level:1,name:x.userName,childrenNum:5,id:x.userName})}),console.log(E)}else A([])})},Pe=function(){var h=z.Z.getRangeStartAndEndTime(7,"YYYY-mm-dd HH:MM:SS"),C=z.Z.getRangeTime(7,"mm-dd");Ee({startTime:h[0],endTime:h[1]}).then(function(E){var x=E.data;P(function(){return x.map(function(B,q){return{year:C[q],value:B}})})})},$e=function(){var R=(0,S.Z)(p().mark(function h(){var C,E;return p().wrap(function(B){for(;;)switch(B.prev=B.next){case 0:return B.next=2,Se();case 2:C=B.sent,E=C.data,Me(E);case 5:case"end":return B.stop()}},h)}));return function(){return R.apply(this,arguments)}}(),Le=function(){var R=(0,S.Z)(p().mark(function h(){return p().wrap(function(E){for(;;)switch(E.prev=E.next){case 0:case"end":return E.stop()}},h)}));return function(){return R.apply(this,arguments)}}(),oe=function(h){(0,Oe.m)(h).then(function(C){var E=C.data;if(E){var x=JSON.parse(E);We(x.total);var B=JSON.parse(x.rows);console.log(x),H(B.map(function(q){return q.id=z.Z.uuid(),q}))}})};return(0,w.useEffect)(function(){ke(),Pe(),oe(re),$e().then(),Le().then(),Ie(),console.log(new Date("2022-07-20T03:22:09.000+0000"))},[]),(0,t.jsxs)("div",{className:k().overview,children:[(0,t.jsx)("div",{className:k().staticWrap,children:(0,t.jsxs)(a.Z,{gutter:20,children:[(0,t.jsx)(u.Z,{span:8,children:(0,t.jsx)(g.Z,{active:!0,loading:N.length===0,children:(0,t.jsx)(Ze.Z,{title:"\u8FD1\u4E03\u5929\u6570\u636E\u6536\u96C6\u60C5\u51B5",chart:(0,t.jsx)("div",{style:{height:"163px"},children:(0,t.jsx)(De,{data:N})})})})}),(0,t.jsx)(u.Z,{span:8,children:(0,t.jsx)(n.Z,{className:k().bgW,title:(0,t.jsx)("div",{className:"text-c",children:"\u6570\u636E\u7EDF\u8BA1\u60C5\u51B5"}),children:(0,t.jsx)(a.Z,{gutter:20,className:"text-c",style:{height:"162px"},children:(0,t.jsxs)(g.Z,{active:!0,loading:N.length===0,children:[(0,t.jsx)(u.Z,{span:12,className:"mt-15 text-c",children:(0,t.jsx)(s.Z,{title:"\u5DF2\u6536\u96C6\u4FE1\u606F",value:Q.visitedInformation,valueStyle:{color:"#3f8600"}})}),(0,t.jsx)(u.Z,{span:12,className:"mt-15",children:(0,t.jsx)(s.Z,{title:"\u6570\u636E\u5E93\u4E2A\u6570",value:Q.dbInstance,valueStyle:{color:"#3f8600"}})}),(0,t.jsx)(u.Z,{span:12,className:"mt-20",children:(0,t.jsx)(s.Z,{title:"\u7528\u6237\u4EBA\u6570",value:Q.user,valueStyle:{color:"#3f8600"}})}),(0,t.jsx)(u.Z,{span:12,className:"mt-20",children:(0,t.jsx)(s.Z,{title:"\u6570\u636E\u5E93\u8868",value:Q.table,valueStyle:{color:"#3f8600"}})})]})})})}),(0,t.jsx)(u.Z,{span:8,children:(0,t.jsx)("div",{className:k().waringWrap,children:(0,t.jsx)(ce,{warningList:ne})})})]})}),(0,t.jsx)("div",{className:k().center,children:(0,t.jsxs)(a.Z,{gutter:30,children:[(0,t.jsx)(u.Z,{span:12,children:(0,t.jsx)(se.ZP,{columns:xe,dataSource:d,scroll:{y:500},options:!1,toolBarRender:function(){return[(0,t.jsx)(D.Z,{type:"link",size:"small",onClick:function(){Z.history.push("/user-access")},children:"\u8BE6\u60C5"},"user-link")]},rowKey:"id",search:!1,pagination:!1,headerTitle:"\u7528\u6237\u8BBF\u95EE\u884C\u4E3A"})}),(0,t.jsx)(u.Z,{span:12,className:"pr-10",children:(0,t.jsx)(g.Z,{active:!0,loading:L.length===0,paragraph:{rows:5},children:(0,t.jsx)(se.ZP,{scroll:{y:500},columns:Ue,dataSource:L,options:!1,rowKey:"id",search:!1,pagination:{total:je,pageSize:re.pageSize,current:re.pageNo,onChange:function(h,C){oe({pageNo:h,pageSize:C})}},toolBarRender:function(){return[(0,t.jsx)(D.Z,{type:"link",size:"small",onClick:function(){Z.history.push("/databases")},children:"\u8BE6\u60C5"},"data-link")]},headerTitle:"\u6570\u636E\u8BBF\u95EE\u884C\u4E3A"})})})]})})]})},we=be},66572:function(G,M,e){"use strict";e.d(M,{m:function(){return a}});var T=e(11849),D=e(3182),I=e(94043),n=e.n(I),_=e(21704);function a(s,i){return l.apply(this,arguments)}function l(){return l=(0,D.Z)(n().mark(function s(i,u){return n().wrap(function(g){for(;;)switch(g.prev=g.next){case 0:return g.abrupt("return",(0,_.WY)("/api/skyflying/getCoarseCountsOfTableName",(0,T.Z)({method:"GET",params:i},u||{})));case 1:case"end":return g.stop()}},s)})),l.apply(this,arguments)}},6650:function(G,M,e){"use strict";e.d(M,{F$:function(){return a},Xd:function(){return s},et:function(){return g},FP:function(){return O},Bx:function(){return p},wA:function(){return ee}});var T=e(11849),D=e(3182),I=e(94043),n=e.n(I),_=e(32773);function a(o){return l.apply(this,arguments)}function l(){return l=(0,D.Z)(n().mark(function o(m){return n().wrap(function(t){for(;;)switch(t.prev=t.next){case 0:return t.abrupt("return",(0,_.WY)("/api/skyflying/getCoarseCountsOfUser",(0,T.Z)({method:"GET"},m||{})));case 1:case"end":return t.stop()}},o)})),l.apply(this,arguments)}function s(o,m){return i.apply(this,arguments)}function i(){return i=(0,D.Z)(n().mark(function o(m,U){return n().wrap(function(r){for(;;)switch(r.prev=r.next){case 0:return r.abrupt("return",(0,_.WY)("/api/skyflying/getCountsOfUserRecentSevenDays",(0,T.Z)({method:"GET",params:m},U||{})));case 1:case"end":return r.stop()}},o)})),i.apply(this,arguments)}function u(o,m){return b.apply(this,arguments)}function b(){return b=_asyncToGenerator(_regeneratorRuntime.mark(function o(m,U){return _regeneratorRuntime.wrap(function(r){for(;;)switch(r.prev=r.next){case 0:return r.abrupt("return",request("/api/skyflying/getCountsOfAllRecentSevenDays",_objectSpread({method:"GET",params:m},U||{})));case 1:case"end":return r.stop()}},o)})),b.apply(this,arguments)}function g(o,m){return S.apply(this,arguments)}function S(){return S=(0,D.Z)(n().mark(function o(m,U){return n().wrap(function(r){for(;;)switch(r.prev=r.next){case 0:return r.abrupt("return",(0,_.WY)("/api/skyflying/getCountsOfUser",(0,T.Z)({method:"GET",params:m},U||{})));case 1:case"end":return r.stop()}},o)})),S.apply(this,arguments)}function O(o,m){return j.apply(this,arguments)}function j(){return j=(0,D.Z)(n().mark(function o(m,U){return n().wrap(function(r){for(;;)switch(r.prev=r.next){case 0:return r.abrupt("return",(0,_.WY)("/api/skyflying/getUserOperationTypeCount",(0,T.Z)({method:"GET",params:m},U||{})));case 1:case"end":return r.stop()}},o)})),j.apply(this,arguments)}function p(o,m){return w.apply(this,arguments)}function w(){return w=(0,D.Z)(n().mark(function o(m,U){return n().wrap(function(r){for(;;)switch(r.prev=r.next){case 0:return r.abrupt("return",(0,_.WY)("/api/skyflying/getUserUsualAndUnusualData",(0,T.Z)({method:"GET",params:m},U||{})));case 1:case"end":return r.stop()}},o)})),w.apply(this,arguments)}function ee(o,m){return k.apply(this,arguments)}function k(){return k=(0,D.Z)(n().mark(function o(m,U){return n().wrap(function(r){for(;;)switch(r.prev=r.next){case 0:return r.abrupt("return",(0,_.WY)("/api/skyflying/getCoarseCountsOfOneUser",(0,T.Z)({method:"GET",params:m||{}},U||{})));case 1:case"end":return r.stop()}},o)})),k.apply(this,arguments)}},2335:function(G,M,e){"use strict";e.d(M,{rO:function(){return a},cD:function(){return s},sF:function(){return u}});var T=e(11849),D=e(3182),I=e(94043),n=e.n(I),_=e(32773);function a(g){return l.apply(this,arguments)}function l(){return l=(0,D.Z)(n().mark(function g(S){return n().wrap(function(j){for(;;)switch(j.prev=j.next){case 0:return j.abrupt("return",(0,_.WY)("/api/skyflying/getUserNameAnomalyDetectionInfo",(0,T.Z)({method:"GET"},S||{})));case 1:case"end":return j.stop()}},g)})),l.apply(this,arguments)}function s(g,S){return i.apply(this,arguments)}function i(){return i=(0,D.Z)(n().mark(function g(S,O){return n().wrap(function(p){for(;;)switch(p.prev=p.next){case 0:return p.abrupt("return",(0,_.WY)("/api/skyflying/getAllAlarmInfoDetailByUserName",(0,T.Z)({method:"GET",params:S||{}},O||{})));case 1:case"end":return p.stop()}},g)})),i.apply(this,arguments)}function u(g,S){return b.apply(this,arguments)}function b(){return b=(0,D.Z)(n().mark(function g(S,O){return n().wrap(function(p){for(;;)switch(p.prev=p.next){case 0:return p.abrupt("return",(0,_.WY)("/api/skyflying/getAnomalyDetectionInfoByGroupByUserName",(0,T.Z)({method:"GET",params:S||{}},O||{})));case 1:case"end":return p.stop()}},g)})),b.apply(this,arguments)}},24480:function(G,M,e){"use strict";e.d(M,{Z:function(){return I}});var T=e(69610),D=e(54941),I=function(){function n(){(0,T.Z)(this,n)}return(0,D.Z)(n,null,[{key:"timeStampToTime",value:function(a){var l=new Date(parseInt(a));return l.toJSON().substring(0,10).replace("T","")}},{key:"getLocalTime",value:function(a){var l=arguments.length>1&&arguments[1]!==void 0?arguments[1]:"YYYY-mm-dd HH:MM:SS",s,i={"Y+":a.getFullYear().toString(),"m+":(a.getMonth()+1).toString(),"d+":a.getDate().toString(),"H+":a.getHours().toString(),"M+":a.getMinutes().toString(),"S+":a.getSeconds().toString()},u=l;for(var b in i)s=new RegExp("("+b+")").exec(l),s&&(u=u.replace(s[1],s[1].length==1?i[b]:i[b].padStart(s[1].length,"0")));return u}},{key:"uuid",value:function(){for(var a=[],l="0123456789abcdef",s=0;s<36;s++)a[s]=l.substr(Math.floor(Math.random()*16),1);return a[14]="4",a[19]=l.substr(a[19]&3|8,1),a[8]=a[13]=a[18]=a[23]="-",a.join("").replace(new RegExp(/(-)/g),"")}},{key:"getRangeTime",value:function(a,l){for(var s=[],i=a-1;i>=0;i--){var u=void 0;i===a-1?u=new Date(Date.now()-i*24*60*60*1e3).setHours(0,0,1,0):u=new Date(Date.now()-i*24*60*60*1e3).setHours(23,59,59,999),l?s.push(n.getLocalTime(new Date(u),l)):s.push(u)}return s}},{key:"getRangeStartAndEndTime",value:function(a,l){for(var s=[],i=a-1;i>=0;i--){var u=void 0;i===a-1?u=new Date(Date.now()-i*24*60*60*1e3).setHours(0,0,0,0):i===0?u=new Date(Date.now()-(i-1)*24*60*60*1e3).setHours(0,0,0,0):u=new Date(Date.now()-i*24*60*60*1e3).setHours(23,59,59,999),l?s.push(n.getLocalTime(new Date(u),l)):s.push(u)}return[s[0],s[a-1]]}}]),n}();I.arrayDeDuplication=function(n){return Array.from(new Set(n))}}}]);
