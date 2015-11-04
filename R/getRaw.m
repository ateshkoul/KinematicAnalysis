function Vidcoords = getRaw(DataStruc,trial,marker)
% Function to extract Raw Data from a specific marker from kinematic marker
% assumes the structure as created by Marco for the kinematic analysis

% ----------------------------
% Author : Atesh Koul (atesh.koul@iit.it)
% 19-05-2015
% RBCS, Italian Institute of technology, Genoa
% ----------------------------

Data =  DataStruc.s.(marker);

Allcoords = Data.xyzf{trial};
% Assuming that T0_samples to Tf_samples are the time points where trial is
% measured
Vidcoords = Allcoords(DataStruc.s.To_samples{trial}:DataStruc.s.Tf_samples{trial},:);

