package com.cosmetic.gg.dto.request.address;

import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.model.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter
@AllArgsConstructor
@NoArgsConstructor
public class AddressSupplierRequest extends BaseModel {

	private String provinceId;

	private String districtId;

	private String wardId;
	
	private String detail;

	private boolean isDefault;
	
	private EStatus status;
	
	private String branch;
	
	private String supplierId;
}
