//============ Copyright (c) Valve Corporation, All rights reserved. ============
//====================== True Open Virtual Reality bridge =======================

#include <openvr_driver.h>
//#include "driverlog.h"

#include <vector>
#include <thread>
#include <chrono>

#include <windows.h>
#include <atlbase.h>

using namespace vr;

#if defined(_WIN32)
#define HMD_DLL_EXPORT extern "C" __declspec( dllexport )
#define HMD_DLL_IMPORT extern "C" __declspec( dllimport )
#elif defined(__GNUC__) || defined(COMPILER_GCC) || defined(__APPLE__)
#define HMD_DLL_EXPORT extern "C" __attribute__((visibility("default")))
#define HMD_DLL_IMPORT extern "C" 
#else
#error "Unsupported Platform."
#endif

inline HmdQuaternion_t HmdQuaternion_Init( double w, double x, double y, double z )
{
	HmdQuaternion_t quat;
	quat.w = w;
	quat.x = x;
	quat.y = y;
	quat.z = z;
	return quat;
}

inline void HmdMatrix_SetIdentity( HmdMatrix34_t *pMatrix )
{
	pMatrix->m[0][0] = 1.f;
	pMatrix->m[0][1] = 0.f;
	pMatrix->m[0][2] = 0.f;
	pMatrix->m[0][3] = 0.f;
	pMatrix->m[1][0] = 0.f;
	pMatrix->m[1][1] = 1.f;
	pMatrix->m[1][2] = 0.f;
	pMatrix->m[1][3] = 0.f;
	pMatrix->m[2][0] = 0.f;
	pMatrix->m[2][1] = 0.f;
	pMatrix->m[2][2] = 1.f;
	pMatrix->m[2][3] = 0.f;
}


// keys for use with the settings API
static const char * const k_pch_Sample_Section = "driver_null";
static const char * const k_pch_Sample_SerialNumber_String = "serialNumber";
static const char * const k_pch_Sample_ModelNumber_String = "modelNumber";
static const char * const k_pch_Sample_WindowX_Int32 = "windowX";
static const char * const k_pch_Sample_WindowY_Int32 = "windowY";
static const char * const k_pch_Sample_WindowWidth_Int32 = "windowWidth";
static const char * const k_pch_Sample_WindowHeight_Int32 = "windowHeight";
static const char * const k_pch_Sample_RenderWidth_Int32 = "renderWidth";
static const char * const k_pch_Sample_RenderHeight_Int32 = "renderHeight";
static const char * const k_pch_Sample_SecondsFromVsyncToPhotons_Float = "secondsFromVsyncToPhotons";
static const char * const k_pch_Sample_DisplayFrequency_Float = "displayFrequency";
static const char * const k_pch_Sample_DistortionK1_Float = "DistortionK1";
static const char * const k_pch_Sample_DistortionK2_Float = "DistortionK2";
static const char * const k_pch_Sample_ZoomWidth_Float = "ZoomWidth";
static const char * const k_pch_Sample_ZoomHeight_Float = "ZoomHeight";
static const char * const k_pch_Sample_DistanceBetweenEyes_Int32 = "DistanceBetweenEyes";
static const char * const k_pch_Sample_ScreenOffsetX_Int32 = "ScreenOffsetX";
static const char * const k_pch_Sample_DebugMode_Bool = "DebugMode";

typedef struct _HMDData
{
	double	X;
	double	Y;
	double	Z;
	double	Yaw;
	double	Pitch;
	double	Roll;
} THMD, *PHMD;

typedef struct _Controller
{
	double	X;
	double	Y;
	double	Z;
	double	Yaw;
	double	Pitch;
	double	Roll;
	WORD	Buttons;
	BYTE	Trigger;
	SHORT	ThumbX;
	SHORT	ThumbY;
} TController, *PController;

typedef DWORD(__stdcall *_GetHMDData)(__out THMD* myHMD);
typedef DWORD(__stdcall *_GetControllersData)(__out TController *myController, __out TController *myController2);
typedef DWORD(__stdcall *_SetControllerData)(__in int dwIndex, __in WORD MotorSpeed);
typedef DWORD(__stdcall *_SetCentering)(__in int dwIndex);

_GetHMDData GetHMDData;
_GetControllersData GetControllersData;
_SetControllerData SetControllerData;
_SetCentering SetCentering;

#define GRIPBTN 0x0001
#define THUMBSTICKBTN 0x0002
#define MENUBTN 0x0004
#define SYSTEMBTN 0x0008

HMODULE hDll;
THMD myHMD;
TController myCtrl, myCtrl2;

bool HMDConnected = false, ctrlsConnected = false;

double DegToRad(double f) {
	return f * (3.14159265358979323846 / 180);
}

//-----------------------------------------------------------------------------
// Purpose:
//-----------------------------------------------------------------------------

class CWatchdogDriver_Sample : public IVRWatchdogProvider
{
public:
	CWatchdogDriver_Sample()
	{
		m_pWatchdogThread = nullptr;
	}

	virtual EVRInitError Init( vr::IVRDriverContext *pDriverContext ) ;
	virtual void Cleanup() ;

private:
	std::thread *m_pWatchdogThread;
};

CWatchdogDriver_Sample g_watchdogDriverNull;


EVRInitError CWatchdogDriver_Sample::Init( vr::IVRDriverContext *pDriverContext )
{
	VR_INIT_WATCHDOG_DRIVER_CONTEXT( pDriverContext );
	//InitDriverLog( vr::VRDriverLog() );

	// Watchdog mode on Windows starts a thread that listens for the 'Y' key on the keyboard to 
	// be pressed. A real driver should wait for a system button event or something else from the 
	// the hardware that signals that the VR system should start up.

	return VRInitError_None;
}


void CWatchdogDriver_Sample::Cleanup()
{
	if (hDll != NULL) FreeLibrary(hDll);
	hDll = nullptr;
	//CleanupDriverLog();
}


//-----------------------------------------------------------------------------
// Purpose:
//-----------------------------------------------------------------------------
class CSampleDeviceDriver : public vr::ITrackedDeviceServerDriver, public vr::IVRDisplayComponent
{
public:
	CSampleDeviceDriver(  )
	{
		m_unObjectId = vr::k_unTrackedDeviceIndexInvalid;
		m_ulPropertyContainer = vr::k_ulInvalidPropertyContainer;

		//DriverLog( "Using settings values\n" );
		m_flIPD = vr::VRSettings()->GetFloat( k_pch_SteamVR_Section, k_pch_SteamVR_IPD_Float );

		char buf[1024];
		vr::VRSettings()->GetString( k_pch_Sample_Section, k_pch_Sample_SerialNumber_String, buf, sizeof( buf ) );
		m_sSerialNumber = buf;

		vr::VRSettings()->GetString( k_pch_Sample_Section, k_pch_Sample_ModelNumber_String, buf, sizeof( buf ) );
		m_sModelNumber = buf;

		m_nWindowX = vr::VRSettings()->GetInt32( k_pch_Sample_Section, k_pch_Sample_WindowX_Int32 );
		m_nWindowY = vr::VRSettings()->GetInt32( k_pch_Sample_Section, k_pch_Sample_WindowY_Int32 );
		m_nWindowWidth = vr::VRSettings()->GetInt32( k_pch_Sample_Section, k_pch_Sample_WindowWidth_Int32 );
		m_nWindowHeight = vr::VRSettings()->GetInt32( k_pch_Sample_Section, k_pch_Sample_WindowHeight_Int32 );
		m_nRenderWidth = vr::VRSettings()->GetInt32( k_pch_Sample_Section, k_pch_Sample_RenderWidth_Int32 );
		m_nRenderHeight = vr::VRSettings()->GetInt32( k_pch_Sample_Section, k_pch_Sample_RenderHeight_Int32 );
		m_flSecondsFromVsyncToPhotons = vr::VRSettings()->GetFloat( k_pch_Sample_Section, k_pch_Sample_SecondsFromVsyncToPhotons_Float );
		m_flDisplayFrequency = vr::VRSettings()->GetFloat( k_pch_Sample_Section, k_pch_Sample_DisplayFrequency_Float );

		m_fDistortionK1 = vr::VRSettings()->GetFloat(k_pch_Sample_Section, k_pch_Sample_DistortionK1_Float);
		m_fDistortionK2 = vr::VRSettings()->GetFloat(k_pch_Sample_Section, k_pch_Sample_DistortionK2_Float);
		m_fZoomWidth = vr::VRSettings()->GetFloat(k_pch_Sample_Section, k_pch_Sample_ZoomWidth_Float);
		m_fZoomHeight = vr::VRSettings()->GetFloat(k_pch_Sample_Section, k_pch_Sample_ZoomHeight_Float);
		m_nDistanceBetweenEyes = vr::VRSettings()->GetInt32(k_pch_Sample_Section, k_pch_Sample_DistanceBetweenEyes_Int32);
		m_nScreenOffsetX = vr::VRSettings()->GetInt32(k_pch_Sample_Section, k_pch_Sample_ScreenOffsetX_Int32);
		m_bDebugMode = vr::VRSettings()->GetBool(k_pch_Sample_Section, k_pch_Sample_DebugMode_Bool);

		/*DriverLog( "driver_null: Serial Number: %s\n", m_sSerialNumber.c_str() );
		DriverLog( "driver_null: Model Number: %s\n", m_sModelNumber.c_str() );
		DriverLog( "driver_null: Window: %d %d %d %d\n", m_nWindowX, m_nWindowY, m_nWindowWidth, m_nWindowHeight );
		DriverLog( "driver_null: Render Target: %d %d\n", m_nRenderWidth, m_nRenderHeight );
		DriverLog( "driver_null: Seconds from Vsync to Photons: %f\n", m_flSecondsFromVsyncToPhotons );
		DriverLog( "driver_null: Display Frequency: %f\n", m_flDisplayFrequency );
		DriverLog( "driver_null: IPD: %f\n", m_flIPD );*/

		CRegKey key;
		TCHAR libPath[MAX_PATH];

		LONG status = key.Open(HKEY_CURRENT_USER, _T("Software\\TrueOpenVR"));
		if (status == ERROR_SUCCESS)
		{
			ULONG libPathSize = sizeof(libPath);

			#ifdef _WIN64
				status = key.QueryStringValue(_T("Library64"), libPath, &libPathSize);
			#else
				status = key.QueryStringValue(_T("Library"), libPath, &libPathSize);
			#endif

			if (status == ERROR_SUCCESS)
			{
				HMDConnected = true;
				hDll = LoadLibrary(libPath);
				GetHMDData = (_GetHMDData)GetProcAddress(hDll, "GetHMDData");
				GetControllersData = (_GetControllersData)GetProcAddress(hDll, "GetControllersData");
				SetControllerData = (_SetControllerData)GetProcAddress(hDll, "SetControllerData");
				SetCentering = (_SetCentering)GetProcAddress(hDll, "SetCentering");

				if (GetHMDData == NULL) HMDConnected = false;
				if (SetCentering == NULL) HMDConnected = false;

				if (GetControllersData != NULL && SetControllerData !=NULL && GetControllersData(&myCtrl, &myCtrl2) == 1)
					ctrlsConnected = true;
			}
		}

		key.Close();
	}

	virtual ~CSampleDeviceDriver()
	{
	}


	virtual EVRInitError Activate( vr::TrackedDeviceIndex_t unObjectId ) 
	{
		m_unObjectId = unObjectId;
		m_ulPropertyContainer = vr::VRProperties()->TrackedDeviceToPropertyContainer( m_unObjectId );


		vr::VRProperties()->SetStringProperty( m_ulPropertyContainer, Prop_ModelNumber_String, m_sModelNumber.c_str() );
		vr::VRProperties()->SetStringProperty( m_ulPropertyContainer, Prop_RenderModelName_String, m_sModelNumber.c_str() );
		vr::VRProperties()->SetFloatProperty( m_ulPropertyContainer, Prop_UserIpdMeters_Float, m_flIPD );
		vr::VRProperties()->SetFloatProperty( m_ulPropertyContainer, Prop_UserHeadToEyeDepthMeters_Float, 0.f );
		vr::VRProperties()->SetFloatProperty( m_ulPropertyContainer, Prop_DisplayFrequency_Float, m_flDisplayFrequency );
		vr::VRProperties()->SetFloatProperty( m_ulPropertyContainer, Prop_SecondsFromVsyncToPhotons_Float, m_flSecondsFromVsyncToPhotons );

		// return a constant that's not 0 (invalid) or 1 (reserved for Oculus)
		vr::VRProperties()->SetUint64Property( m_ulPropertyContainer, Prop_CurrentUniverseId_Uint64, 2 );

		// avoid "not fullscreen" warnings from vrmonitor
		vr::VRProperties()->SetBoolProperty( m_ulPropertyContainer, Prop_IsOnDesktop_Bool, false );

		//Debug mode activate Windowed Mode (borderless fullscreen), lock to 30 FPS 
		vr::VRProperties()->SetBoolProperty(m_ulPropertyContainer, Prop_DisplayDebugMode_Bool, m_bDebugMode);

		// Icons can be configured in code or automatically configured by an external file "drivername\resources\driver.vrresources".
		// Icon properties NOT configured in code (post Activate) are then auto-configured by the optional presence of a driver's "drivername\resources\driver.vrresources".
		// In this manner a driver can configure their icons in a flexible data driven fashion by using an external file.
		//
		// The structure of the driver.vrresources file allows a driver to specialize their icons based on their HW.
		// Keys matching the value in "Prop_ModelNumber_String" are considered first, since the driver may have model specific icons.
		// An absence of a matching "Prop_ModelNumber_String" then considers the ETrackedDeviceClass ("HMD", "Controller", "GenericTracker", "TrackingReference")
		// since the driver may have specialized icons based on those device class names.
		//
		// An absence of either then falls back to the "system.vrresources" where generic device class icons are then supplied.
		//
		// Please refer to "bin\drivers\sample\resources\driver.vrresources" which contains this sample configuration.
		//
		// "Alias" is a reserved key and specifies chaining to another json block.
		//
		// In this sample configuration file (overly complex FOR EXAMPLE PURPOSES ONLY)....
		//
		// "Model-v2.0" chains through the alias to "Model-v1.0" which chains through the alias to "Model-v Defaults".
		//
		// Keys NOT found in "Model-v2.0" would then chase through the "Alias" to be resolved in "Model-v1.0" and either resolve their or continue through the alias.
		// Thus "Prop_NamedIconPathDeviceAlertLow_String" in each model's block represent a specialization specific for that "model".
		// Keys in "Model-v Defaults" are an example of mapping to the same states, and here all map to "Prop_NamedIconPathDeviceOff_String".
		//
		bool bSetupIconUsingExternalResourceFile = true;
		if ( !bSetupIconUsingExternalResourceFile )
		{
			// Setup properties directly in code.
			// Path values are of the form {drivername}\icons\some_icon_filename.png
			vr::VRProperties()->SetStringProperty( m_ulPropertyContainer, vr::Prop_NamedIconPathDeviceOff_String, "{null}/icons/headset_sample_status_off.png" );
			vr::VRProperties()->SetStringProperty( m_ulPropertyContainer, vr::Prop_NamedIconPathDeviceSearching_String, "{null}/icons/headset_sample_status_searching.gif" );
			vr::VRProperties()->SetStringProperty( m_ulPropertyContainer, vr::Prop_NamedIconPathDeviceSearchingAlert_String, "{null}/icons/headset_sample_status_searching_alert.gif" );
			vr::VRProperties()->SetStringProperty( m_ulPropertyContainer, vr::Prop_NamedIconPathDeviceReady_String, "{null}/icons/headset_sample_status_ready.png" );
			vr::VRProperties()->SetStringProperty( m_ulPropertyContainer, vr::Prop_NamedIconPathDeviceReadyAlert_String, "{null}/icons/headset_sample_status_ready_alert.png" );
			vr::VRProperties()->SetStringProperty( m_ulPropertyContainer, vr::Prop_NamedIconPathDeviceNotReady_String, "{null}/icons/headset_sample_status_error.png" );
			vr::VRProperties()->SetStringProperty( m_ulPropertyContainer, vr::Prop_NamedIconPathDeviceStandby_String, "{null}/icons/headset_sample_status_standby.png" );
			vr::VRProperties()->SetStringProperty( m_ulPropertyContainer, vr::Prop_NamedIconPathDeviceAlertLow_String, "{null}/icons/headset_sample_status_ready_low.png" );
		}

		return VRInitError_None;
	}

	virtual void Deactivate() 
	{
		m_unObjectId = vr::k_unTrackedDeviceIndexInvalid;
	}

	virtual void EnterStandby()
	{
	}

	void *GetComponent( const char *pchComponentNameAndVersion )
	{
		if ( !_stricmp( pchComponentNameAndVersion, vr::IVRDisplayComponent_Version ) )
		{
			return (vr::IVRDisplayComponent*)this;
		}

		// override this to add a component to a driver
		return NULL;
	}

	virtual void PowerOff() 
	{
	}

	/** debug request from a client */
	virtual void DebugRequest( const char *pchRequest, char *pchResponseBuffer, uint32_t unResponseBufferSize ) 
	{
		if( unResponseBufferSize >= 1 )
			pchResponseBuffer[0] = 0;
	}

	virtual void GetWindowBounds( int32_t *pnX, int32_t *pnY, uint32_t *pnWidth, uint32_t *pnHeight ) 
	{
		*pnX = m_nWindowX;
		*pnY = m_nWindowY;
		*pnWidth = m_nWindowWidth;
		*pnHeight = m_nWindowHeight;
	}

	virtual bool IsDisplayOnDesktop() 
	{
		return true;
	}

	virtual bool IsDisplayRealDisplay() 
	{
		return true;
	}

	virtual void GetRecommendedRenderTargetSize( uint32_t *pnWidth, uint32_t *pnHeight ) 
	{
		*pnWidth = m_nRenderWidth;
		*pnHeight = m_nRenderHeight;
	}

	virtual void GetEyeOutputViewport( EVREye eEye, uint32_t *pnX, uint32_t *pnY, uint32_t *pnWidth, uint32_t *pnHeight ) 
	{
		/**pnY = 0;
		*pnWidth = m_nWindowWidth / 2;
		*pnHeight = m_nWindowHeight;
	
		if ( eEye == Eye_Left )
		{
			*pnX = 0;
		}
		else
		{
			*pnX = m_nWindowWidth / 2;
		}*/
		*pnY = m_nScreenOffsetX;
		*pnWidth = m_nWindowWidth / 2;
		*pnHeight = m_nWindowHeight;

		if (eEye == Eye_Left)
		{
			*pnX = m_nDistanceBetweenEyes;
		}
		else
		{
			*pnX = (m_nWindowWidth / 2) - m_nDistanceBetweenEyes;
		}
	}

	virtual void GetProjectionRaw( EVREye eEye, float *pfLeft, float *pfRight, float *pfTop, float *pfBottom ) 
	{
		*pfLeft = -1.0;
		*pfRight = 1.0;
		*pfTop = -1.0;
		*pfBottom = 1.0;	
	}

	virtual DistortionCoordinates_t ComputeDistortion( EVREye eEye, float fU, float fV ) 
	{
		DistortionCoordinates_t coordinates;

		//distortion for lens from https://github.com/HelenXR/openvr_survivor/blob/master/src/head_mount_display_device.cc
		float hX;
		float hY;
		double rr;
		double r2;
		double theta;

		rr = sqrt((fU - 0.5f)*(fU - 0.5f) + (fV - 0.5f)*(fV - 0.5f));
		r2 = rr * (1 + m_fDistortionK1 * (rr*rr) + m_fDistortionK2 * (rr*rr*rr*rr));
		theta = atan2(fU - 0.5f, fV - 0.5f);
		hX = sin(theta)*r2*m_fZoomWidth;
		hY = cos(theta)*r2*m_fZoomHeight;

		coordinates.rfBlue[0] = hX + 0.5f;
		coordinates.rfBlue[1] = hY + 0.5f;
		coordinates.rfGreen[0] = hX + 0.5f;
		coordinates.rfGreen[1] = hY + 0.5f;
		coordinates.rfRed[0] = hX + 0.5f;
		coordinates.rfRed[1] = hY + 0.5f;

		return coordinates;
	}

	virtual DriverPose_t GetPose() 
	{
		DriverPose_t pose = { 0 };

		if (HMDConnected) {
			pose.poseIsValid = true;
			pose.result = TrackingResult_Running_OK;
			pose.deviceIsConnected = true;
		}
		else
		{
			pose.poseIsValid = false;
			pose.result = TrackingResult_Uninitialized;
			pose.deviceIsConnected = false;
		}

		pose.qWorldFromDriverRotation = HmdQuaternion_Init(1, 0, 0, 0);
		pose.qDriverFromHeadRotation = HmdQuaternion_Init(1, 0, 0, 0);

		if (HMDConnected) {
			if ((GetAsyncKeyState(VK_NUMPAD5) & 0x8000) != 0 || ((GetAsyncKeyState(VK_CONTROL) & 0x8000) != 0 && (GetAsyncKeyState(VK_MENU) & 0x8000) != 0 && (GetAsyncKeyState(82) & 0x8000) != 0))
				SetCentering(0);

			GetHMDData(&myHMD);

			//Set head tracking rotation
			pose.qRotation.w = cos(DegToRad(myHMD.Yaw) * 0.5) * cos(DegToRad(myHMD.Roll) * 0.5) * cos(DegToRad(myHMD.Pitch) * 0.5) + sin(DegToRad(myHMD.Yaw) * 0.5) * sin(DegToRad(myHMD.Roll) * 0.5) * sin(DegToRad(myHMD.Pitch) * 0.5);
			pose.qRotation.x = cos(DegToRad(myHMD.Yaw) * 0.5) * sin(DegToRad(myHMD.Roll) * 0.5) * cos(DegToRad(myHMD.Pitch) * 0.5) - sin(DegToRad(myHMD.Yaw) * 0.5) * cos(DegToRad(myHMD.Roll) * 0.5) * sin(DegToRad(myHMD.Pitch) * 0.5);
			pose.qRotation.y = cos(DegToRad(myHMD.Yaw) * 0.5) * cos(DegToRad(myHMD.Roll) * 0.5) * sin(DegToRad(myHMD.Pitch) * 0.5) + sin(DegToRad(myHMD.Yaw) * 0.5) * sin(DegToRad(myHMD.Roll) * 0.5) * cos(DegToRad(myHMD.Pitch) * 0.5);
			pose.qRotation.z = sin(DegToRad(myHMD.Yaw) * 0.5) * cos(DegToRad(myHMD.Roll) * 0.5) * cos(DegToRad(myHMD.Pitch) * 0.5) - cos(DegToRad(myHMD.Yaw) * 0.5) * sin(DegToRad(myHMD.Roll) * 0.5) * sin(DegToRad(myHMD.Pitch) * 0.5);

			//Set position tracking
			pose.vecPosition[0] = myHMD.X;
			pose.vecPosition[1] = myHMD.Y;
			pose.vecPosition[2] = myHMD.Z;
		}

		return pose;
	}
	

	void RunFrame()
	{
		// In a real driver, this should happen from some pose tracking thread.
		// The RunFrame interval is unspecified and can be very irregular if some other
		// driver blocks it for some periodic task.
		if ( m_unObjectId != vr::k_unTrackedDeviceIndexInvalid )
		{
			vr::VRServerDriverHost()->TrackedDevicePoseUpdated( m_unObjectId, GetPose(), sizeof( DriverPose_t ) );
		}
	}

	std::string GetSerialNumber() const { return m_sSerialNumber; }

private:
	vr::TrackedDeviceIndex_t m_unObjectId;
	vr::PropertyContainerHandle_t m_ulPropertyContainer;

	std::string m_sSerialNumber;
	std::string m_sModelNumber;

	int32_t m_nWindowX;
	int32_t m_nWindowY;
	int32_t m_nWindowWidth;
	int32_t m_nWindowHeight;
	int32_t m_nRenderWidth;
	int32_t m_nRenderHeight;
	float m_flSecondsFromVsyncToPhotons;
	float m_flDisplayFrequency;
	float m_flIPD;
	float m_fDistortionK1;
	float m_fDistortionK2;
	float m_fZoomWidth;
	float m_fZoomHeight;
	int32_t m_nDistanceBetweenEyes;
	int32_t m_nScreenOffsetX;
	bool m_bDebugMode;
};

//-----------------------------------------------------------------------------
// Purpose:
//-----------------------------------------------------------------------------

class CSampleControllerDriver : public vr::ITrackedDeviceServerDriver
{
	int32_t ControllerIndex;
public:
	CSampleControllerDriver()
	{
		m_unObjectId = vr::k_unTrackedDeviceIndexInvalid;
		m_ulPropertyContainer = vr::k_ulInvalidPropertyContainer;
	}

	virtual void SetControllerIndex(int32_t CtrlIndex)
	{
		ControllerIndex = CtrlIndex;
	}

	virtual ~CSampleControllerDriver()
	{
	}

	virtual EVRInitError Activate( vr::TrackedDeviceIndex_t unObjectId )
	{
		m_unObjectId = unObjectId;
		m_ulPropertyContainer = vr::VRProperties()->TrackedDeviceToPropertyContainer( m_unObjectId );

		vr::VRProperties()->SetStringProperty(m_ulPropertyContainer, vr::Prop_ControllerType_String, "vive_controller");
		vr::VRProperties()->SetStringProperty(m_ulPropertyContainer, vr::Prop_LegacyInputProfile_String, "vive_controller");

		vr::VRProperties()->SetStringProperty(m_ulPropertyContainer, vr::Prop_ModelNumber_String, "ViveMV");
		vr::VRProperties()->SetStringProperty(m_ulPropertyContainer, vr::Prop_ManufacturerName_String, "HTC");
		vr::VRProperties()->SetStringProperty(m_ulPropertyContainer, vr::Prop_RenderModelName_String, "vr_controller_vive_1_5");

		vr::VRProperties()->SetStringProperty(m_ulPropertyContainer, Prop_TrackingSystemName_String, "VR Controller");
		vr::VRProperties()->SetInt32Property(m_ulPropertyContainer, Prop_DeviceClass_Int32, TrackedDeviceClass_Controller);

		switch (ControllerIndex)
		{
		case 1:
			vr::VRProperties()->SetStringProperty(m_ulPropertyContainer, Prop_SerialNumber_String, "CTRL1Serial");
			break;
		case 2:
			vr::VRProperties()->SetStringProperty(m_ulPropertyContainer, Prop_SerialNumber_String, "CTRL2Serial");
			break;
		}
		
		uint64_t supportedButtons = 0xFFFFFFFFFFFFFFFFULL;
		vr::VRProperties()->SetUint64Property(m_ulPropertyContainer, vr::Prop_SupportedButtons_Uint64, supportedButtons);

		// return a constant that's not 0 (invalid) or 1 (reserved for Oculus)
		//vr::VRProperties()->SetUint64Property( m_ulPropertyContainer, Prop_CurrentUniverseId_Uint64, 2 );

		// avoid "not fullscreen" warnings from vrmonitor
		//vr::VRProperties()->SetBoolProperty( m_ulPropertyContainer, Prop_IsOnDesktop_Bool, false );

		// our sample device isn't actually tracked, so set this property to avoid having the icon blink in the status window
		//vr::VRProperties()->SetBoolProperty( m_ulPropertyContainer, Prop_NeverTracked_Bool, false );

		// even though we won't ever track we want to pretend to be the right hand so binding will work as expected

		switch (ControllerIndex)
		{
		case 1:
			vr::VRProperties()->SetInt32Property(m_ulPropertyContainer, Prop_ControllerRoleHint_Int32, TrackedControllerRole_LeftHand);
			break;
		case 2:
			vr::VRProperties()->SetInt32Property(m_ulPropertyContainer, Prop_ControllerRoleHint_Int32, TrackedControllerRole_RightHand);
			break;
		}
			

		// this file tells the UI what to show the user for binding this controller as well as what default bindings should
		// be for legacy or other apps
		vr::VRProperties()->SetStringProperty( m_ulPropertyContainer, Prop_InputProfilePath_String, "{null}/input/mycontroller_profile.json" );

		//  Buttons handles
		vr::VRDriverInput()->CreateBooleanComponent(m_ulPropertyContainer, "/input/application_menu/click", &HButtons[0]);
		vr::VRDriverInput()->CreateBooleanComponent(m_ulPropertyContainer, "/input/grip/click", &HButtons[1]);
		vr::VRDriverInput()->CreateBooleanComponent(m_ulPropertyContainer, "/input/system/click", &HButtons[2]);
		vr::VRDriverInput()->CreateBooleanComponent(m_ulPropertyContainer, "/input/trackpad/click", &HButtons[3]);
		vr::VRDriverInput()->CreateBooleanComponent(m_ulPropertyContainer, "/input/trackpad/touch", &HButtons[4]);

		// Analog handles
		vr::VRDriverInput()->CreateScalarComponent(
			m_ulPropertyContainer, "/input/trackpad/x", &HAnalog[0],
			vr::VRScalarType_Absolute, vr::VRScalarUnits_NormalizedTwoSided
		);
		vr::VRDriverInput()->CreateScalarComponent(
			m_ulPropertyContainer, "/input/trackpad/y", &HAnalog[1],
			vr::VRScalarType_Absolute, vr::VRScalarUnits_NormalizedTwoSided
		);
		vr::VRDriverInput()->CreateScalarComponent(
			m_ulPropertyContainer, "/input/trigger/value", &HAnalog[2],
			vr::EVRScalarType::VRScalarType_Absolute, vr::EVRScalarUnits::VRScalarUnits_NormalizedOneSided
		);

		vr::VRProperties()->SetInt32Property(m_ulPropertyContainer, vr::Prop_Axis0Type_Int32, vr::k_eControllerAxis_TrackPad);

		// create our haptic component
		vr::VRDriverInput()->CreateHapticComponent( m_ulPropertyContainer, "/output/haptic", &m_compHaptic);

		return VRInitError_None;
	}

	virtual void Deactivate()
	{
		m_unObjectId = vr::k_unTrackedDeviceIndexInvalid;
	}


	virtual void EnterStandby()
	{
	}

	void *GetComponent( const char *pchComponentNameAndVersion )
	{
		// override this to add a component to a driver
		return NULL;
	}

	virtual void PowerOff()
	{
	}

	/** debug request from a client */
	virtual void DebugRequest( const char *pchRequest, char *pchResponseBuffer, uint32_t unResponseBufferSize )
	{
		if ( unResponseBufferSize >= 1 )
			pchResponseBuffer[0] = 0;
	}

	virtual DriverPose_t GetPose()
	{
		DriverPose_t pose = { 0 };
		//pose.poseIsValid = false;
		pose.poseIsValid = true;
		//pose.result = TrackingResult_Calibrating_OutOfRange;
		pose.result = TrackingResult_Running_OK;
		pose.deviceIsConnected = true;

		pose.qWorldFromDriverRotation = HmdQuaternion_Init( 1, 0, 0, 0 );
		pose.qDriverFromHeadRotation = HmdQuaternion_Init( 1, 0, 0, 0 );

		//Controllers positions and rotations
		if (ControllerIndex == 1) {

			pose.vecPosition[0] = myCtrl.X;
			pose.vecPosition[1] = myCtrl.Y;
			pose.vecPosition[2] = myCtrl.Z;

			//Convert yaw, pitch, roll to quaternion
			pose.qRotation.w = cos(DegToRad(myCtrl.Yaw) * 0.5) * cos(DegToRad(myCtrl.Roll) * 0.5) * cos(DegToRad(myCtrl.Pitch) * 0.5) + sin(DegToRad(myCtrl.Yaw) * 0.5) * sin(DegToRad(myCtrl.Roll) * 0.5) * sin(DegToRad(myCtrl.Pitch) * 0.5);
			pose.qRotation.x = cos(DegToRad(myCtrl.Yaw) * 0.5) * sin(DegToRad(myCtrl.Roll) * 0.5) * cos(DegToRad(myCtrl.Pitch) * 0.5) - sin(DegToRad(myCtrl.Yaw) * 0.5) * cos(DegToRad(myCtrl.Roll) * 0.5) * sin(DegToRad(myCtrl.Pitch) * 0.5);
			pose.qRotation.y = cos(DegToRad(myCtrl.Yaw) * 0.5) * cos(DegToRad(myCtrl.Roll) * 0.5) * sin(DegToRad(myCtrl.Pitch) * 0.5) + sin(DegToRad(myCtrl.Yaw) * 0.5) * sin(DegToRad(myCtrl.Roll) * 0.5) * cos(DegToRad(myCtrl.Pitch) * 0.5);
			pose.qRotation.z = sin(DegToRad(myCtrl.Yaw) * 0.5) * cos(DegToRad(myCtrl.Roll) * 0.5) * cos(DegToRad(myCtrl.Pitch) * 0.5) - cos(DegToRad(myCtrl.Yaw) * 0.5) * sin(DegToRad(myCtrl.Roll) * 0.5) * sin(DegToRad(myCtrl.Pitch) * 0.5);

		} else { 
			//Controller2
			pose.vecPosition[0] = myCtrl2.X;
			pose.vecPosition[1] = myCtrl2.Y;
			pose.vecPosition[2] = myCtrl2.Z;

			pose.qRotation.w = cos(DegToRad(myCtrl2.Yaw) * 0.5) * cos(DegToRad(myCtrl2.Roll) * 0.5) * cos(DegToRad(myCtrl2.Pitch) * 0.5) + sin(DegToRad(myCtrl2.Yaw) * 0.5) * sin(DegToRad(myCtrl2.Roll) * 0.5) * sin(DegToRad(myCtrl2.Pitch) * 0.5);
			pose.qRotation.x = cos(DegToRad(myCtrl2.Yaw) * 0.5) * sin(DegToRad(myCtrl2.Roll) * 0.5) * cos(DegToRad(myCtrl2.Pitch) * 0.5) - sin(DegToRad(myCtrl2.Yaw) * 0.5) * cos(DegToRad(myCtrl2.Roll) * 0.5) * sin(DegToRad(myCtrl2.Pitch) * 0.5);
			pose.qRotation.y = cos(DegToRad(myCtrl2.Yaw) * 0.5) * cos(DegToRad(myCtrl2.Roll) * 0.5) * sin(DegToRad(myCtrl2.Pitch) * 0.5) + sin(DegToRad(myCtrl2.Yaw) * 0.5) * sin(DegToRad(myCtrl2.Roll) * 0.5) * cos(DegToRad(myCtrl2.Pitch) * 0.5);
			pose.qRotation.z = sin(DegToRad(myCtrl2.Yaw) * 0.5) * cos(DegToRad(myCtrl2.Roll) * 0.5) * cos(DegToRad(myCtrl2.Pitch) * 0.5) - cos(DegToRad(myCtrl2.Yaw) * 0.5) * sin(DegToRad(myCtrl2.Roll) * 0.5) * sin(DegToRad(myCtrl2.Pitch) * 0.5);
		}

		return pose;
	}

	double ConvAxis(double n) {
		if (n > 1) {
			return 1;
		}
		else if (n < -1)
		{
			return -1;
		}
		else
		{
			return n;
		}
	}

	void RunFrame()
	{
	#if defined( _WINDOWS )
		// Your driver would read whatever hardware state is associated with its input components and pass that
		// in to UpdateBooleanComponent. This could happen in RunFrame or on a thread of your own that's reading USB
		// state. There's no need to update input state unless it changes, but it doesn't do any harm to do so.
		
		if (ControllerIndex == 1) {

			vr::VRDriverInput()->UpdateBooleanComponent(HButtons[0], (myCtrl.Buttons & MENUBTN) != 0, 0); //Application Menu
			vr::VRDriverInput()->UpdateBooleanComponent(HButtons[1], (myCtrl.Buttons & GRIPBTN) != 0, 0); //Grip
			vr::VRDriverInput()->UpdateBooleanComponent(HButtons[2], (myCtrl.Buttons & SYSTEMBTN) != 0, 0); //System
			vr::VRDriverInput()->UpdateBooleanComponent(HButtons[3], (myCtrl.Buttons & THUMBSTICKBTN) != 0, 0); //Trackpad

			vr::VRDriverInput()->UpdateScalarComponent(HAnalog[0], ConvAxis(myCtrl.ThumbX * 0.00003051758), 0); //Trackpad x
			vr::VRDriverInput()->UpdateScalarComponent(HAnalog[1], ConvAxis(myCtrl.ThumbY * 0.00003051758), 0); //Trackpad y

			if (myCtrl.ThumbX != 0 || myCtrl.ThumbY != 0) {
				vr::VRDriverInput()->UpdateBooleanComponent(HButtons[4], 1, 0); //Trackpad touch
			}
			else {
				vr::VRDriverInput()->UpdateBooleanComponent(HButtons[4], 0, 0); //Trackpad touch
			}

			vr::VRDriverInput()->UpdateScalarComponent(HAnalog[2], ConvAxis(myCtrl.Trigger * 0.003921568627451), 0); //Trigger
		} else {
			//Controller2
			vr::VRDriverInput()->UpdateBooleanComponent(HButtons[0], (myCtrl2.Buttons & MENUBTN) != 0, 0); //Application Menu
			vr::VRDriverInput()->UpdateBooleanComponent(HButtons[1], (myCtrl2.Buttons & GRIPBTN) != 0, 0); //Grip
			vr::VRDriverInput()->UpdateBooleanComponent(HButtons[2], (myCtrl2.Buttons & SYSTEMBTN) != 0, 0); //System
			vr::VRDriverInput()->UpdateBooleanComponent(HButtons[3], (myCtrl2.Buttons & THUMBSTICKBTN) != 0, 0); //Trackpad

			vr::VRDriverInput()->UpdateScalarComponent(HAnalog[0], ConvAxis(myCtrl2.ThumbX * 0.00003051758), 0); //Trackpad x
			vr::VRDriverInput()->UpdateScalarComponent(HAnalog[1], ConvAxis(myCtrl2.ThumbY * 0.00003051758), 0); //Trackpad y

			if (myCtrl2.ThumbX != 0 || myCtrl2.ThumbY != 0) {
				vr::VRDriverInput()->UpdateBooleanComponent(HButtons[4], 1, 0); //Trackpad touch
			}
			else {
				vr::VRDriverInput()->UpdateBooleanComponent(HButtons[4], 0, 0); //Trackpad touch
			}

			vr::VRDriverInput()->UpdateScalarComponent(HAnalog[2], ConvAxis(myCtrl2.Trigger * 0.003921568627451), 0); //Trigger
		}

		//Centring
		if ((myCtrl.Buttons & SYSTEMBTN) && (myCtrl.Buttons & GRIPBTN) && (myCtrl2.Buttons & SYSTEMBTN) && (myCtrl2.Buttons & GRIPBTN))
		{
			SetCentering(0);
			SetCentering(1);
			SetCentering(2);
		}

		if (m_unObjectId != vr::k_unTrackedDeviceIndexInvalid)
		{
			vr::VRServerDriverHost()->TrackedDevicePoseUpdated(m_unObjectId, GetPose(), sizeof(DriverPose_t));
		}

#endif
	}

	void ProcessEvent( const vr::VREvent_t & vrEvent )
	{
		switch ( vrEvent.eventType )
		{
		case vr::VREvent_Input_HapticVibration:
		{
			if ( vrEvent.data.hapticVibration.componentHandle == m_compHaptic )
			{
				SetControllerData(ControllerIndex, 21845); 
				// This is where you would send a signal to your hardware to trigger actual haptic feedback
			}
		}
		break;
		}
	}

	std::string GetSerialNumber() const { 
		
		switch (ControllerIndex)
		{
		case 1:
			return "CTRL1Serial";
			break;
		case 2:
			return "CTRL2Serial";
			break;
		}
	}

private:
	vr::TrackedDeviceIndex_t m_unObjectId;
	vr::PropertyContainerHandle_t m_ulPropertyContainer;

	//vr::VRInputComponentHandle_t m_compA;
	//vr::VRInputComponentHandle_t m_compB;
	//vr::VRInputComponentHandle_t m_compC;
	vr::VRInputComponentHandle_t m_compHaptic;

	vr::VRInputComponentHandle_t HButtons[5], HAnalog[3];
	//std::string m_sSerialNumber;
	//std::string m_sModelNumber;
};

//-----------------------------------------------------------------------------
// Purpose:
//-----------------------------------------------------------------------------
class CServerDriver_Sample: public IServerTrackedDeviceProvider
{
public:
	virtual EVRInitError Init( vr::IVRDriverContext *pDriverContext ) ;
	virtual void Cleanup() ;
	virtual const char * const *GetInterfaceVersions() { return vr::k_InterfaceVersions; }
	virtual void RunFrame() ;
	virtual bool ShouldBlockStandbyMode()  { return false; }
	virtual void EnterStandby()  {}
	virtual void LeaveStandby()  {}

private:
	CSampleDeviceDriver *m_pNullHmdLatest = nullptr;
	CSampleControllerDriver *m_pController = nullptr;
	CSampleControllerDriver *m_pController2 = nullptr;
};

CServerDriver_Sample g_serverDriverNull;


EVRInitError CServerDriver_Sample::Init( vr::IVRDriverContext *pDriverContext )
{
	VR_INIT_SERVER_DRIVER_CONTEXT( pDriverContext );
	//InitDriverLog( vr::VRDriverLog() );

	m_pNullHmdLatest = new CSampleDeviceDriver();
	vr::VRServerDriverHost()->TrackedDeviceAdded( m_pNullHmdLatest->GetSerialNumber().c_str(), vr::TrackedDeviceClass_HMD, m_pNullHmdLatest );

	if (ctrlsConnected) {
		m_pController = new CSampleControllerDriver();
		m_pController->SetControllerIndex(1);
		vr::VRServerDriverHost()->TrackedDeviceAdded(m_pController->GetSerialNumber().c_str(), vr::TrackedDeviceClass_Controller, m_pController);


		m_pController2 = new CSampleControllerDriver();
		m_pController2->SetControllerIndex(2);
		vr::VRServerDriverHost()->TrackedDeviceAdded(m_pController2->GetSerialNumber().c_str(), vr::TrackedDeviceClass_Controller, m_pController2);
	}

	return VRInitError_None;
}

void CServerDriver_Sample::Cleanup() 
{
	if (hDll != NULL) FreeLibrary(hDll);
	hDll = nullptr;
	//CleanupDriverLog();
	delete m_pNullHmdLatest;
	m_pNullHmdLatest = NULL;
	delete m_pController;
	m_pController = NULL;
	delete m_pController2;
	m_pController2 = NULL;
}


void CServerDriver_Sample::RunFrame()
{
	if ( m_pNullHmdLatest )
	{
		m_pNullHmdLatest->RunFrame();
	}
	if (ctrlsConnected) {
		GetControllersData(&myCtrl, &myCtrl2);

		if (m_pController)
		{
			m_pController->RunFrame();
		}
		if (m_pController2)
		{
			m_pController2->RunFrame();
		}

		vr::VREvent_t vrEvent;
		while ( vr::VRServerDriverHost()->PollNextEvent( &vrEvent, sizeof( vrEvent ) ) )
		{
			if ( m_pController )
			{
				m_pController->ProcessEvent(vrEvent);
			}
			if (m_pController2)
			{
				m_pController2->ProcessEvent(vrEvent);
			}
		}
	}
}

//-----------------------------------------------------------------------------
// Purpose:
//-----------------------------------------------------------------------------
HMD_DLL_EXPORT void *HmdDriverFactory( const char *pInterfaceName, int *pReturnCode )
{
	if( 0 == strcmp( IServerTrackedDeviceProvider_Version, pInterfaceName ) )
	{
		return &g_serverDriverNull;
	}
	if( 0 == strcmp( IVRWatchdogProvider_Version, pInterfaceName ) )
	{
		return &g_watchdogDriverNull;
	}

	if( pReturnCode )
		*pReturnCode = VRInitError_Init_InterfaceNotFound;

	return NULL;
}

