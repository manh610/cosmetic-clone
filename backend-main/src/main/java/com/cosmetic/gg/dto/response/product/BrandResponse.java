package com.cosmetic.gg.dto.response.product;

import java.time.LocalDateTime;

import javax.persistence.Lob;

import com.cosmetic.gg.common.enums.EStatus;
import com.cosmetic.gg.model.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter
@AllArgsConstructor
@NoArgsConstructor
public class BrandResponse extends BaseModel{
	
	private String id;

	private String code;

	private String name;
	
	private String country;

	@Lob
	private byte[] logo;

	private String slogan;

	private boolean isMall = false;
	
	private EStatus status;

	private LocalDateTime createdAt;

	private String createdBy;
	
	private LocalDateTime updatedAt;

	private String updatedBy;
	
	private String description;
	
	private Integer totalSell;
}
